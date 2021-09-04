FROM ubuntu:rolling

RUN set -ex; \
    apt-get update; \
    DEBIAN_FRONTEND=noninteractive apt-get install --yes -o APT::Install-Suggests=false --no-install-recommends \
        git \
        curl \
        gnupg2 \
        xz-utils \
        sudo \
        openjdk-11-jre-headless; \
    rm -rf /var/lib/apt/lists/*; \
    useradd -ms /bin/bash sroot; \
    echo 'sroot ALL=(ALL:ALL) NOPASSWD: ALL' > /etc/sudoers.d/sroot;
ADD /payload.sh /
RUN chmod +x /payload.sh
USER sroot

RUN set -ex; \
    cd /home/sroot/; \
    curl -o install-nix-2.3.12 https://releases.nixos.org/nix/nix-2.3.12/install; \
    curl -o install-nix-2.3.12.asc https://releases.nixos.org/nix/nix-2.3.12/install.asc; \
    curl -o edolstra.gpg https://nixos.org/edolstra.gpg; \
    gpg2 --import edolstra.gpg; \
    gpg2 --verify ./install-nix-2.3.12.asc; \
    mkdir -p ~/.config/nix/; \
    echo 'sandbox = false' > ~/.config/nix/nix.conf; \
    sh ./install-nix-2.3.12; \
    git clone https://github.com/hexresearch/ergvein; \
    cd ergvein; \
    git checkout v25;

WORKDIR /home/sroot/ergvein/

ENTRYPOINT ["/bin/bash", "/payload.sh"]