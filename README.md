# Ergvein

[![Build Status](https://travis-ci.com/hexresearch/ergvein.svg?branch=master)](https://travis-ci.com/hexresearch/ergvein)

Ergvein is cryptocurrency [SPV](https://bitcoin.org/en/operating-modes-guide#simplified-payment-verification-spv) wallet that is focused on privacy, decentralized exchange and heavy usage of smart contracts. The wallet supports several currencies and provides means to perform atomic swaps between them. The project encourages users to host their own backend node. That allows node owner to get part of fees from atomic swaps and promotes privacy.

Features:
* Supported currencies: [Bitcoin](https://bitcoin.org/en/), [Ergo](https://ergoplatform.org/en/), [ZCash](https://z.cash/), [USDT](https://tether.to/), [EOS](https://eos.io/), [ETH](https://ethereum.org/).
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

# Troubleshooting

## Element inspector fails to render

If the page turns to blank when you open the element inspector or resize the window try exporting
`export WEBKIT_DISABLE_COMPOSITING_MODE=1` before running the app.
