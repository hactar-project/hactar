# a nix shell for bootstrapping copilot to work on hactar
let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  sbclWithAsdf = pkgs.sbcl;
  quicklispLisp = pkgs.fetchurl {
    url = "https://beta.quicklisp.org/quicklisp.lisp";
    sha256 = "sha256-SnpcKuvgcWQXBHhUJnOX4kpE0MzglhJ0EenOnM/rLBc=";
  };
  commonPackages = with pkgs; [
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
    gcc
  ];
  devPackages = with pkgs; [
    readline.dev
    libuv.dev
    openssl.dev
    zlib.dev
    libyaml.dev
    libuuid.dev
    libpng.dev
    libjpeg.dev
    libtiff.dev
    sqlite.dev
  
  ];
  sandbox = import (fetchTarball
    "https://github.com/archie-judd/agent-sandbox.nix/archive/main.tar.gz") {
      pkgs = pkgs;
    };
  copilot-sandboxed = sandbox.mkSandbox {
    pkg = pkgs.github-copilot-cli;
    binName = "copilot";
    outName = "copilot-sandboxed";
    allowedPackages = with pkgs; [
      coreutils
      which
      gnumake
      git
      ripgrep
      fd
      gnused
      gnugrep
      findutils
      diffutils
      less
      gawk
      jq
    ] ++ commonPackages ++ devPackages;
    stateDirs = [ "$HOME/.config/github-copilot" "$HOME/.copilot" "$HOME/quicklisp"];
    stateFiles = [ ];
    extraEnv = {
      GITHUB_TOKEN = "$GITHUB_TOKEN";
      GIT_AUTHOR_NAME = "copilot";
      GIT_AUTHOR_EMAIL = "copilot@localhost";
      GIT_COMMITTER_NAME = "copilot";
      GIT_COMMITTER_EMAIL = "copilot@localhost";
    };
    restrictNetwork = true;
    allowedDomains = {
      "githubcopilot.com" = "*";
      "github.com" = "*";
      "githubusercontent.com" = [ "GET" "HEAD" ];
    };

  };

  bash-sandboxed = sandbox.mkSandbox {
    pkg = pkgs.bashInteractive;
    binName = "bash";
    outName = "bash-sandboxed";
    allowedPackages = with pkgs; [
      coreutils
      which
      gnumake
      git
      ripgrep
      fd
      gnused
      gnugrep
      findutils
      diffutils
      less
      gawk
      jq
    ] ++ commonPackages ++ devPackages;
    stateDirs = [ "$HOME/.config/github-copilot" "$HOME/.copilot" "$HOME/quicklisp" ];
    stateFiles = [ ];
    restrictNetwork = true;
    extraEnv = {
      GITHUB_TOKEN = "$GITHUB_TOKEN";
      GIT_AUTHOR_NAME = "copilot";
      GIT_AUTHOR_EMAIL = "copilot@localhost";
      GIT_COMMITTER_NAME = "copilot";
      GIT_COMMITTER_EMAIL = "copilot@localhost";
      LD_LIBRARY_PATH = "$LD_LIBRARY_PATH";
      SBCL_HOME = "$SBCL_HOME";
      CL_SOURCE_REGISTRY = "$CL_SOURCE_REGISTRY";
      CPATH = "$CPATH";
      C_INCLUDE_PATH = "$C_INCLUDE_PATH";
      CPLUS_INCLUDE_PATH = "$CPLUS_INCLUDE_PATH";
      LIBRARY_PATH = "$LIBRARY_PATH";
      PKG_CONFIG_PATH = "$PKG_CONFIG_PATH";
      NIX_CFLAGS_COMPILE = "$NIX_CFLAGS_COMPILE";
      NIX_LDFLAGS = "$NIX_LDFLAGS";
      NIX_CC_WRAPPER_TARGET_HOST_x86_64_unknown_linux_gnu = "$NIX_CC_WRAPPER_TARGET_HOST_x86_64_unknown_linux_gnu";
      NIX_CC_WRAPPER_TARGET_BUILD_x86_64_unknown_linux_gnu = "$NIX_CC_WRAPPER_TARGET_BUILD_x86_64_unknown_linux_gnu";
      NIX_CC_WRAPPER_TARGET_TARGET_x86_64_unknown_linux_gnu = "$NIX_CC_WRAPPER_TARGET_TARGET_x86_64_unknown_linux_gnu";
    };
    allowedDomains = {
      "githubcopilot.com" = "*";
      "github.com" = "*";
      "githubusercontent.com" = [ "GET" "HEAD" ];
    };
  };

in pkgs.mkShell {
  packages = [ copilot-sandboxed bash-sandboxed ];

  buildInputs = commonPackages;

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

    export CL_SOURCE_REGISTRY="$PWD//:$CL_SOURCE_REGISTRY"
    export SBCL_HOME="${sbclWithAsdf}/lib/sbcl"

    # Bootstrap Quicklisp into the home dir so the sandbox can load it offline.
    # This runs in the outer (networked) nix-shell; the sandbox itself only
    # reads the already-installed ~/quicklisp/setup.lisp.
    if [ ! -f "$HOME/quicklisp/setup.lisp" ]; then
      echo "Bootstrapping Quicklisp into $HOME/quicklisp ..."
      sbcl --non-interactive \
        --eval "(require :asdf)" \
        --load "${quicklispLisp}" \
        --eval "(quicklisp-quickstart:install :path \"$HOME/quicklisp/\")" \
        --quit
    fi

    echo "Common Lisp development environment loaded with readline support"
  '';
}
