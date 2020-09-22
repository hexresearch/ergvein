# Ergvein

![Build all](https://github.com/hexresearch/ergvein/workflows/Build%20all/badge.svg)

Ergvein is MVP implementation for [Cypra](https://cypra.io) ecosystem. It is cryptocurrency [SPV](https://bitcoin.org/en/operating-modes-guide#simplified-payment-verification-spv) wallet that is focused on privacy, decentralized exchange and heavy usage of smart contracts. The wallet supports several currencies and provides means to perform atomic swaps between them. The project encourages users to host their own backend node. That allows node owner to get part of fees from atomic swaps and promotes privacy.

**Wallet is not finished yet! Consider the product as alpha quality and subject for changes.**

Features:
* Supported currencies: [Bitcoin](https://bitcoin.org/en/), [Ergo](https://ergoplatform.org/en/), [Litecoin](https://litecoin.org/), [ZCash](https://z.cash/), [USDT](https://tether.to/) (Omni protocol).
* We use [SPV](https://bitcoin.org/en/operating-modes-guide#simplified-payment-verification-spv) design. Wallet doesn't trust nodes. All data is double checked: the wallet samples multiple nodes and checks results for consistency.
* Private keys are stored in encrypted storage and never leave your device. All wallet data is encrypted and protected by your password.
* Built-in atomic swaps, exchange your crypto in one click.
* Built-in decentralized coin mixing: non interactive [SNICKER](https://joinmarket.me/blog/blog/snicker/) for BTC and [ErgoMix](https://ergoplatform.org/docs/AdvancedErgoScriptTutorial.pdf) for Ergo.
* Mobile and desktop applications
* Decentralized public network of indexing nodes that allows users to make money when they run them.

# How to build project

1. You need `nix` tool to build the repo. Install with non-root user:
```
bash <(curl https://nixos.org/nix/install)
```
1. Optional, to speed up (factor of 10 times) build, you can enable cachix binary cache. See secion below.
1. To build desktop version:
```
./shells.sh
cabal new-build all
cabal new-run ergvein
```
1. To build android version:
```
./make-android.sh
./install-apk.sh
```
1. If you are using NixOS you need to manually enable reflex-platform cache:
```
nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
```

# How to use cachix

We provide binary cache for builds via cachix.org. To start using it to speed up your builds:
```
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use ergvein
```

# Deploying indexer server

You can use our NixOS module to deploy index server in ten minutes. Add the following to your `/etc/nixos/configuration.nix`:
``` nix
let ergvein = pkgs.fetchFromGitHub {
      owner = "hexresearch";
      repo = "ergvein";
      rev = "d2cd3670cf981e502c6310876487a285ed469ab3";
      sha256 = "0l718lfcmz5687cffhikcg0pvhph15m6ixab0xy83x006m3ajfwz";
    };
in {
  imports = [
    "${ergvein}/nix/modules/ergvein.nix"
    "${ergvein}/nix/modules/local-secrets.nix" # remove if using nixops secrets
  ];
  services.ergvein = {
    enable = true;
    externalAddress = externalAddress = { host = "127.0.0.1"; port = 8667; }; # here place your ip
  };
  deployment.keys = {
    btcpassword.text = "verysecretpassword";
  };

}
```

# UI prototyping

* Start the server:
    * ``cd ui-playground``
    * ``./watch.sh``
* Implement the design in pure HTML+CSS in ``index.html``
* You may use ``css/extra.css`` for fast CSS changes
* Alternatevly use Style.hs to mimic the Ergvein.Style module with Clay
   1. Change Style.hs
   2. ``./generate-css.sh``
   3. Sometimes hakyll fails to detect changes from the ``generate-css`` script. In this case, open ``css/style.css`` and press Ctrl+S

The page is accessible at ``127.0.0.1:8000``

Finally, transfer design choices and extra classes to the wallet.

# Troubleshooting

## Element inspector fails to render

If the page turns to blank when you open the element inspector or resize the window try exporting
`export WEBKIT_DISABLE_COMPOSITING_MODE=1` before running the app.

## pg_ctl failed to start

Error: `could not create lock file "/var/run/postgresql/.s.PGSQL.5434.lock": Permission denied`
Solution: `sudo chmod a+w /var/run/postgresql`
