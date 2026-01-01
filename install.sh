#!/usr/bin/env bash
set -euo pipefail

# Hactar installer
# 1. If Roswell is already installed: ros install hactarproject/hactar
# 2. Otherwise: install Roswell (and its build deps), then ros install hactarproject/hactar
# 3. Run hactar hactar.init

has_cmd() {
  command -v "$1" >/dev/null 2>&1
}

is_nixos() {
  [ -f /etc/os-release ] && grep -qi '^ID=nixos' /etc/os-release
}

detect_manager() {
  if has_cmd apt-get; then echo "apt"; return; fi
  if has_cmd dnf; then echo "dnf"; return; fi
  if has_cmd yum; then echo "yum"; return; fi
  if has_cmd pacman; then echo "pacman"; return; fi
  if has_cmd zypper; then echo "zypper"; return; fi
  if has_cmd apk; then echo "apk"; return; fi
  if has_cmd brew; then echo "brew"; return; fi
  echo ""
}

get_sudo() {
  local mgr="$1"
  if [ "$mgr" = "brew" ]; then
    echo ""
    return
  fi
  if [ "${EUID:-$(id -u)}" -ne 0 ]; then
    if has_cmd sudo; then
      echo "sudo"
      return
    else
      echo "Warning: Installing dependencies may require root privileges but 'sudo' is not available." >&2
    fi
  fi
  echo ""
}

install_roswell_build_deps() {
  local mgr="$1"
  local SUDO
  SUDO="$(get_sudo "$mgr")"

  echo "Installing Roswell build dependencies..."
  case "$mgr" in
    apt)
      $SUDO apt update -y
      $SUDO apt install -y gcc make curl libcurl4-openssl-dev automake autoconf \
        sqlite3 libsqlite3-dev libuv1-dev libev-dev libncurses-dev libncurses6 libncursesw6 || true
      ;;
    dnf)
      $SUDO dnf install -y gcc make curl libcurl-devel automake autoconf \
        sqlite sqlite-devel libuv libuv-devel libev-devel ncurses-devel || true
      ;;
    yum)
      $SUDO yum install -y gcc make curl libcurl-devel automake autoconf \
        sqlite sqlite-devel libuv libuv-devel libev-devel ncurses-devel || true
      ;;
    brew)
      brew update
      for p in automake autoconf curl sqlite libuv libev ncurses; do
        if ! brew list --versions "$p" >/dev/null 2>&1; then
          brew install "$p" || true
        fi
      done
      ;;
    zypper)
      $SUDO zypper -n refresh
      $SUDO zypper -n install gcc make curl libcurl-devel automake autoconf \
        sqlite3 sqlite3-devel libuv-devel libev-devel ncurses-devel || true
      ;;
    pacman)
      $SUDO pacman -Syu --noconfirm
      $SUDO pacman -S --noconfirm --needed gcc make curl automake autoconf \
        sqlite libuv libev ncurses || true
      ;;
    apk)
      $SUDO apk update || true
      $SUDO apk add --no-cache gcc make musl-dev curl curl-dev automake autoconf \
        sqlite sqlite-dev libuv-dev libev-dev ncurses-dev || true
      ;;
    *)
      echo "Warning: Unknown package manager '$mgr'. Please install gcc, make, curl, automake, autoconf, sqlite, libuv, libev, and libncurses manually." >&2
      ;;
  esac
}

install_roswell_from_source() {
  echo "Building Roswell from source (last resort)..."

  local mgr
  mgr="$(detect_manager)"

  # Install build dependencies
  if [ -n "$mgr" ]; then
    install_roswell_build_deps "$mgr"
  else
    echo "Warning: Could not detect a package manager. Ensure gcc, make, curl, automake, and autoconf are installed." >&2
  fi

  local tmp_dir
  tmp_dir="$(mktemp -d)"
  (
    cd "$tmp_dir"
    echo "Downloading Roswell source..."
    if has_cmd curl; then
      curl -fsSL "https://github.com/roswell/roswell/releases/latest/download/roswell.tar.gz" -o roswell.tar.gz
    elif has_cmd wget; then
      wget -qO roswell.tar.gz "https://github.com/roswell/roswell/releases/latest/download/roswell.tar.gz"
    else
      echo "Error: Neither 'curl' nor 'wget' is available." >&2
      exit 1
    fi

    tar -xzf roswell.tar.gz
    cd roswell-*/ 2>/dev/null || cd roswell/
    ./configure --prefix="$HOME/.roswell/local"
    make
    make install
  )
  rm -rf "$tmp_dir"

  export PATH="$HOME/.roswell/local/bin:$PATH"

  if ! has_cmd ros; then
    echo "Error: Roswell build from source failed. 'ros' not found after install." >&2
    return 1
  fi

  echo "Running Roswell setup..."
  ros setup
}

install_roswell_via_deb() {
  echo "Installing Roswell via .deb package..."
  local SUDO
  SUDO="$(get_sudo "apt")"

  local tmp_dir
  tmp_dir="$(mktemp -d)"

  # Ensure curl and jq (or fallback) are available
  $SUDO apt-get update -y
  $SUDO apt-get install -y curl || true

  local deb_url=""
  # Try to get latest .deb URL via GitHub API
  if has_cmd jq; then
    deb_url="$(curl -s https://api.github.com/repos/roswell/roswell/releases/latest \
      | jq -r '.assets | .[] | select(.name|test("\\.deb$")) | .browser_download_url' 2>/dev/null || true)"
  fi

  # Fallback: try a known recent version
  if [ -z "$deb_url" ]; then
    deb_url="$(curl -s https://api.github.com/repos/roswell/roswell/releases/latest \
      | grep -o '"browser_download_url": *"[^"]*\.deb"' \
      | head -1 \
      | sed 's/"browser_download_url": *"//;s/"$//' || true)"
  fi

  if [ -z "$deb_url" ]; then
    echo "Could not determine .deb download URL." >&2
    rm -rf "$tmp_dir"
    return 1
  fi

  echo "Downloading: $deb_url"
  curl -fsSL "$deb_url" -o "$tmp_dir/roswell.deb"
  $SUDO dpkg -i "$tmp_dir/roswell.deb" || $SUDO apt-get install -f -y
  rm -rf "$tmp_dir"

  if has_cmd ros; then
    echo "Running Roswell setup..."
    ros setup
    return 0
  fi

  return 1
}

is_debian_based() {
  [ -f /etc/debian_version ] || ([ -f /etc/os-release ] && grep -qiE '^ID(_LIKE)?=.*(debian|ubuntu)' /etc/os-release)
}

install_roswell() {
  echo "Installing Roswell..."

  local mgr
  mgr="$(detect_manager)"

  # 1. Try native package manager (distros that package roswell)
  case "$mgr" in
    brew)
      echo "Trying: brew install roswell"
      brew install roswell || true
      if has_cmd ros; then ros setup; return 0; fi
      ;;
    pacman)
      echo "Trying: pacman -S roswell"
      local SUDO
      SUDO="$(get_sudo "$mgr")"
      $SUDO pacman -Syu --noconfirm
      $SUDO pacman -S --noconfirm roswell || true
      if has_cmd ros; then ros setup; return 0; fi
      ;;
    apk)
      echo "Trying: apk add roswell (edge/testing)"
      local SUDO
      SUDO="$(get_sudo "$mgr")"
      $SUDO apk add --repository=http://dl-cdn.alpinelinux.org/alpine/edge/testing roswell || true
      if has_cmd ros; then ros setup; return 0; fi
      ;;
  esac

  # 2. On Debian/Ubuntu, try the .deb release
  if is_debian_based; then
    install_roswell_via_deb && has_cmd ros && return 0
    echo "Deb install did not succeed, falling back..."
  fi

  # 3. If brew is available (Linux Homebrew) but wasn't the primary manager, try it
  if [ "$mgr" != "brew" ] && has_cmd brew; then
    echo "Trying: brew install roswell (linuxbrew)"
    brew install roswell || true
    if has_cmd ros; then ros setup; return 0; fi
  fi

  # 4. Last resort: build from source
  install_roswell_from_source
}

install_hactar() {
  echo "Installing Hactar via Roswell..."
  ros install hactarproject/hactar
}

run_hactar_init() {
  echo "Running initial setup: hactar hactar.init"

  # Check PATH first
  if has_cmd hactar; then
    hactar hactar.init || true
    return 0
  fi

  # Check common Roswell bin location
  if [ -x "$HOME/.roswell/bin/hactar" ]; then
    "$HOME/.roswell/bin/hactar" hactar.init || true
    return 0
  fi

  echo "Warning: Could not locate 'hactar' on PATH." >&2
  echo "Ensure ~/.roswell/bin is on your PATH:" >&2
  echo "  export PATH=\"\$HOME/.roswell/bin:\$PATH\"" >&2
}

install_via_nix() {
  echo "Detected NixOS. Installing via nix profile..."
  if ! has_cmd nix; then
    echo "Error: 'nix' command not found on this system, cannot install via nix." >&2
    return 1
  fi
  nix profile install "github:hactar-project/hactar"
}

show_help() {
  cat <<EOF
Hactar Installer

Installs Hactar via Roswell. If Roswell is not found, it will be
installed first along with its build dependencies (gcc, make, etc.).
On NixOS, installs via nix profile instead.

Usage: $0 [OPTIONS]

Options:
  -h, --help       Show this help message and exit

Environment Variables:
  HACTAR_BIN_DIR       (unused, reserved for future use)
EOF
}

main() {
  for arg in "$@"; do
    if [ "$arg" = "-h" ] || [ "$arg" = "--help" ]; then
      show_help
      exit 0
    fi
  done

  # NixOS: use nix profile
  if is_nixos; then
    install_via_nix
    run_hactar_init
    echo
    echo "Hactar installation complete (Nix)."
    exit 0
  fi

  # Install Roswell if not present
  if ! has_cmd ros; then
    install_roswell
  else
    echo "Roswell found: $(which ros)"
  fi

  # Install Hactar
  install_hactar

  # Run init
  run_hactar_init

  echo
  echo "Hactar installation complete."
  echo "Ensure ~/.roswell/bin is on your PATH:"
  echo "  export PATH=\"\$HOME/.roswell/bin:\$PATH\""
}

main "$@"

