#!/usr/bin/env bash

set -eo pipefail

NIX_VERSION="2.7.0"
NIX_INSTALL_URL="https://releases.nixos.org/nix/nix-${NIX_VERSION}/install"
NIX_INSTALL_SHA256="f18f315faab1cccc4b0ea0dcdf577dfce237238f5a3893f3bc67db4806af39c1"
NIX_INSTALL_PATH="/tmp/nix-install-${NIX_VERSION}"

echo "Setting up Nix package manager..."

# Download installer and verify SHA256
curl -sSf "${NIX_INSTALL_URL}" -o "${NIX_INSTALL_PATH}"
echo "${NIX_INSTALL_SHA256} ${NIX_INSTALL_PATH}" | sha256sum -c
chmod +x "${NIX_INSTALL_PATH}"

"${NIX_INSTALL_PATH}"

if [[ $? -eq 0 ]]; then
    echo "The Nix package manager was successfully installed."
else
    echo "Failed to install Nix package manager!" >&2
    exit 1
fi