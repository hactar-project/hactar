# a sandbox for locking down hactar
{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:

let
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

  hactar-pkg = pkgs.writeShellScriptBin "hactar" ''
    if [ -f ./hactar ]; then
      exec ./hactar "$@"
    else
      echo "Error: './hactar' executable not found in current directory." >&2
      echo "Please build it first by running 'make build' inside the nix shell." >&2
      exit 1
    fi
  '';

  hactar-sandboxed = sandbox.mkSandbox {
    pkg = hactar-pkg;
    binName = "hactar";
    outName = "hactar-sandboxed";
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
    stateDirs = [
      "$HOME/.config/hactar"
      "$HOME/.local/share/hactar"
      "$HOME/.local/share/hactar-repo"
      "$HOME/quicklisp"
      "$HOME/.config/github-copilot"
      "$HOME/.copilot"
    ];
    stateFiles = [ ];
    restrictNetwork = true;
    allowedDomains = {
      "github.com" = "*";
      "api.github.com" = "*";
      "githubcopilot.com" = "*";
      "api.githubcopilot.com" = "*";
      "api.openai.com" = "*";
      "api.anthropic.com" = "*";
      "openrouter.ai" = "*";
      "api.mistral.ai" = "*";
      "api.deepseek.com" = "*";
      "api.moonshot.ai" = "*";
      "opencode.ai" = "*";
      "amazonaws.com" = "*";
      "generativelanguage.googleapis.com" = "*";
      "claude.ai" = "*";
      "console.anthropic.com" = "*";
      "auth.openai.com" = "*";
      "accounts.google.com" = "*";
      "oauth2.googleapis.com" = "*";
      "www.googleapis.com" = "*";
      "cloudcode-pa.googleapis.com" = "*";
    };
    extraEnv = {
      OPENAI_API_KEY = "$OPENAI_API_KEY";
      ANTHROPIC_API_KEY = "$ANTHROPIC_API_KEY";
      GEMINI_API_KEY = "$GEMINI_API_KEY";
      OPENROUTER_API_KEY = "$OPENROUTER_API_KEY";
      MISTRAL_API_KEY = "$MISTRAL_API_KEY";
      DEEPSEEK_API_KEY = "$DEEPSEEK_API_KEY";
      AWS_ACCESS_KEY_ID = "$AWS_ACCESS_KEY_ID";
      AWS_SECRET_ACCESS_KEY = "$AWS_SECRET_ACCESS_KEY";
      AWS_DEFAULT_REGION = "$AWS_DEFAULT_REGION";
      GITHUB_TOKEN = "$GITHUB_TOKEN";
      GIT_AUTHOR_NAME = "hactar";
      GIT_AUTHOR_EMAIL = "hactar@localhost";
      GIT_COMMITTER_NAME = "hactar";
      GIT_COMMITTER_EMAIL = "hactar@localhost";
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
    stateDirs = [
      "$HOME/.config/hactar"
      "$HOME/.local/share/hactar"
      "$HOME/.local/share/hactar-repo"
      "$HOME/quicklisp"
      "$HOME/.config/github-copilot"
      "$HOME/.copilot"
    ];
    stateFiles = [ ];
    restrictNetwork = true;
    allowedDomains = {
      "github.com" = "*";
      "api.github.com" = "*";
      "githubcopilot.com" = "*";
      "api.githubcopilot.com" = "*";
      "api.openai.com" = "*";
      "api.anthropic.com" = "*";
      "openrouter.ai" = "*";
      "api.mistral.ai" = "*";
      "api.deepseek.com" = "*";
      "api.moonshot.ai" = "*";
      "opencode.ai" = "*";
      "amazonaws.com" = "*";
      "generativelanguage.googleapis.com" = "*";
      "claude.ai" = "*";
      "console.anthropic.com" = "*";
      "auth.openai.com" = "*";
      "accounts.google.com" = "*";
      "oauth2.googleapis.com" = "*";
      "www.googleapis.com" = "*";
      "cloudcode-pa.googleapis.com" = "*";
    };
    extraEnv = {
      OPENAI_API_KEY = "$OPENAI_API_KEY";
      ANTHROPIC_API_KEY = "$ANTHROPIC_API_KEY";
      GEMINI_API_KEY = "$GEMINI_API_KEY";
      OPENROUTER_API_KEY = "$OPENROUTER_API_KEY";
      MISTRAL_API_KEY = "$MISTRAL_API_KEY";
      DEEPSEEK_API_KEY = "$DEEPSEEK_API_KEY";
      AWS_ACCESS_KEY_ID = "$AWS_ACCESS_KEY_ID";
      AWS_SECRET_ACCESS_KEY = "$AWS_SECRET_ACCESS_KEY";
      AWS_DEFAULT_REGION = "$AWS_DEFAULT_REGION";
      GITHUB_TOKEN = "$GITHUB_TOKEN";
      GIT_AUTHOR_NAME = "hactar";
      GIT_AUTHOR_EMAIL = "hactar@localhost";
      GIT_COMMITTER_NAME = "hactar";
      GIT_COMMITTER_EMAIL = "hactar@localhost";
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
  };

in pkgs.mkShell {
  packages = [ hactar-sandboxed bash-sandboxed ];

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

    echo "=========================================================="
    echo "Hactar Sandboxed Development Environment Loaded!"
    echo "  - Run 'make build' to compile the hactar executable"
    echo "  - Run 'hactar-sandboxed' to run hactar inside the sandbox"
    echo "  - Run 'bash-sandboxed' to start a sandboxed shell session"
    echo "=========================================================="
  '';
}
