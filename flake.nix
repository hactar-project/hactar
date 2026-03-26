{
  description = "Hactar AI Pair Programmer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        nativeLibs = with pkgs; [ openssl sqlite libuv libyaml libev cmark ];

        nhooks = pkgs.sbcl.buildASDFSystem rec {
          pname = "nhooks";
          version = "1.2.2";
          src = pkgs.fetchFromGitHub {
            owner = "atlas-engineer";
            repo = "nhooks";
            rev = version;
            hash = "sha256-6A3fsemsv2KbTmdGMQeL9iHFUBHc4kK6CRNVyc91LdU=";
          };
          lispLibs = with pkgs.sbclPackages; [
            bordeaux-threads
            closer-mop
            serapeum
          ];
        };

        lispLibs = with pkgs.sbclPackages; [
          cl-ppcre
          cxml
          uuid
          sqlite
          chunga
          cl-readline
          cl-yaml
          serapeum
          #  uiop
          cl-mustache
          fuzzy-match
          cl-toml
          cl-async
          str
          nhooks
          usocket
          lparallel
          cffi
          with-user-abort
          cl-json
          cl-fad
          shasht
          local-time
          flexi-streams
          alexandria
          cl-base64
          slynk
          clingon
          ningle
          woo
          clack
          lack
          imago
          dexador
          drakma
          babel
          fiveam
          mockingbird
        ];

        sbcl = pkgs.sbcl.withPackages (ps: lispLibs);

        hactar = pkgs.stdenv.mkDerivation {
          pname = "hactar";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = [ pkgs.makeWrapper ];
          buildInputs = [ sbcl ] ++ nativeLibs;

          dontStrip = true;

          buildPhase = ''
            export XDG_CACHE_HOME=$(pwd)/.cache
            export LD_LIBRARY_PATH=${
              pkgs.lib.makeLibraryPath nativeLibs
            }:$LD_LIBRARY_PATH

            ${sbcl}/bin/sbcl --no-userinit --no-sysinit --non-interactive \
              --load build.lisp \
              --eval "(build-hactar)"
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp hactar $out/bin/

            wrapProgram $out/bin/hactar \
              --prefix LD_LIBRARY_PATH : ${pkgs.lib.makeLibraryPath nativeLibs}
          '';
        };

      in {
        packages.hactar = hactar;
        packages.default = hactar;

        devShells.default = pkgs.mkShell {
          buildInputs = [ sbcl pkgs.sqlite-utils pkgs.rlwrap pkgs.cmark ] ++ nativeLibs;

          shellHook = ''
            export CL_SOURCE_REGISTRY=$(pwd)''${CL_SOURCE_REGISTRY:+:$CL_SOURCE_REGISTRY}
            export LD_LIBRARY_PATH=${
              pkgs.lib.makeLibraryPath nativeLibs
            }:$LD_LIBRARY_PATH
          '';
        };
      });
}
