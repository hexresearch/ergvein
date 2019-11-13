# Ergvein

Ergvein is cryptocurrency [SPV](https://bitcoin.org/en/operating-modes-guide#simplified-payment-verification-spv) wallet that is focused on privacy, decentralized exchange and heavy usage of smart contracts. The wallet supports several currencies and provides means to perform atomic swaps between them. The project encourages users to host their own backend node. That allows node owner to get part of fees from atomic swaps and promotes privacy.

Features:
* Supported currencies: [Bitcoin](https://bitcoin.org/en/), [Ergo](https://ergoplatform.org/en/), [ZCash](https://z.cash/), [USDT](https://tether.to/), [EOS](https://eos.io/), [ETH](https://ethereum.org/).
* We use [SPV](https://bitcoin.org/en/operating-modes-guide#simplified-payment-verification-spv) design. Wallet doesn't trust
nodes. All data is double checked: the wallet samples multiple nodes and checks results for consistensy.
* Private keys are stored in encrypted storage and never leave your device. All wallet data is encrypted and protected by your password.
* Built-in atomic swaps, exchange your crypto in one click.
* Built-in decentralized coin mixing: [CoinJoin](https://en.bitcoin.it/wiki/CoinJoin) for BTC and [ErgoMix](https://ergoplatform.org/docs/AdvancedErgoScriptTutorial.pdf) for Ergo.
* Mobile and desktop applications
