{ pkgs ? import <nixpkgs> { } }:

let
  sbclWithAsdf = pkgs.sbcl;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    readline
    libuv
    libev
    rlwrap
    pkg-config
    openssl
    zlib
    libyaml
    sbclWithAsdf
    git
    gnumake
    cacert
    libuuid
    libpng
    libjpeg
    libtiff
    sqlite
    sqlite-vec
    sqlite-utils
    cmark
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${
      pkgs.lib.makeLibraryPath [
        pkgs.readline
        pkgs.libyaml
        pkgs.libuv
        pkgs.libev
        pkgs.sqlite-vec
        pkgs.sqlite
        pkgs.openssl
        pkgs.cmark
      ]
    }:$LD_LIBRARY_PATH

    # Set up ASDF to find local systems
    export CL_SOURCE_REGISTRY="$PWD//:$CL_SOURCE_REGISTRY"
    export SBCL_HOME="${sbclWithAsdf}/lib/sbcl"

    echo "Common Lisp development environment loaded with readline support"
  '';
}
