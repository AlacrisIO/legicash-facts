# Bibliography

* [Plasma](http://plasma.io),
  [ETH Research on Plasma](https://ethresear.ch/search?q=plasma),
  [Construction of a Plasma Chain 0x1](https://blog.omisego.network/construction-of-a-plasma-chain-0x1-614f6ebd1612) ([MVP git](https://github.com/omisego/plasma-mvp.git)),
  [Joseph Poon's talk at Deconomy 2018: "Consensus and Cryptoeconomic Incentive Mechanisms"](https://youtu.be/nZKdy7kZGBc).

* [Efficiently Bridging EVM Blockchains Relay Networks V2](https://blog.gridplus.io/efficiently-bridging-evm-blockchains-8421504e9ced) by Alex Miller

* [Alice and Bob](https://en.wikipedia.org/wiki/Alice_and_Bob)

* OCaml style guides:
 [Jane Street](https://opensource.janestreet.com/standards/),
 [OCaml.org](https://ocaml.org/learn/tutorials/guidelines.html),
 [INRIA](https://caml.inria.fr/resources/doc/guides/guidelines.en.html),
 [Xen Hypervisor](https://wiki.xenproject.org/wiki/OCaml_Best_Practices_for_Developers),
 [Decision Procedure Toolkit](http://dpt.sourceforge.net/conventions.html),
 [Cornell CS3110](https://www.cs.cornell.edu/courses/cs3110/2011sp/Handouts/style.htm),
 [UPenn CIS341](https://www.seas.upenn.edu/~cis341/current/programming_style.shtml),
 [Brown CS017](https://cs.brown.edu/courses/cs017/content/docs/ocaml-style.pdf).

* OCaml @ Jane Street:
 [documentation](https://ocaml.janestreet.com/ocaml-core/latest/doc/index.html),
 [OCaml Labs](http://ocamllabs.io/).

* OCaml metaprogramming:
 [Oleg's Reconciling Abstraction with High Performance: A MetaOCaml approach](https://www.nowpublishers.com/article/Details/PGL-038).

* [Computability Logic](http://www.csc.villanova.edu/~japaridz/CL/)

* [K Framework](http://www.kframework.org/index.php/Main_Page),
  [KEVM: Semantics of EVM in K](https://github.com/kframework/evm-semantics).

* Session Types for conversations:
 * [Parametrized Extensible Effects and Session Types](http://okmij.org/ftp/Haskell/extensible/param-eff.pdf)
   by Oleg Kiselyov,
 * [Acute](http://www.cl.cam.ac.uk/~pes20/acute/) and its successor
   [HashCaml](http://www.cl.cam.ac.uk/~pes20/hashcaml/) by Peter Sewell.
   Interesting, but they "only" address naming and marshalling,
   and with a weak security model.
 * [Cryptographic Protocol Explication and End-Point Projection](https://cs.brown.edu/~sk/Publications/Papers/Published/mk-crypto-prot-expl-epp/)
   by Jay McCarthy and Shriram Krishnamurthi (2008), or that cites it,
   [Cryptographic protocol synthesis and verification for multiparty sessions](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/01/cryptographic-protocol-synthesis-and-verification-for-multiparty-sessions-csf09.pdf)
   by Karthikeyan Bhargavan, Ricardo Corin, Pierre-malo Deniélou, Cédric Fournet, James J. Leifer (2009).
   Or [things that cite the latter](http://citeseerx.ist.psu.edu/showciting?doi=10.1.1.156.187).

* Byzantine Fault Tolerance:
 [Papers selected by Rick Dudley](https://medium.com/@AFDudley/byzantine-fault-tolerant-consensus-papers-1b4b47d27463) (2015)

* Scaling:
 [Ethereum wiki Sharding FAQ](https://github.com/ethereum/wiki/wiki/Sharding-FAQ),
 [Ethereum Sharding introduction and implementations](https://github.com/ethereum/wiki/wiki/Sharding-introduction-and-implementations).

* Oracles:
 [Truthcoin on Oracles vs Contracts](http://www.truthcoin.info/blog/contracts-oracles-sidechains/)
 (lots of great ideas, lots of more dubious ideas; great food for thought).

* Contracts in Tezos:
  [Michelson: the language of Smart Contracts in Tezos (PDF)](https://www.tezos.com/static/papers/language.pdf),
  [Michelson-lang.com](https://www.michelson-lang.com/),
  [Tezos Forum on Smart contracts](https://forums.tezos.community/c/smart-contracts),
  [Michelson contracts need access to blockchain state](https://gitlab.com/tezos/tezos/issues/158),
  [Watching the Tezos blockchain](https://github.com/MiloDavis/Hacky-OCaml-to-connect-to-Tezos-node).

* Contracts in EVM:
  [opcodes](https://ethereum.stackexchange.com/questions/119/what-opcodes-are-available-for-the-ethereum-evm),
  [old Ethereum block hashes](https://github.com/amiller/ethereum-blockhashes),
  [Security Audit by QuantStamp](https://quantstamp.com/start),
  [Towards verifying ethereum smart contract bytecode in Isabelle/HOL](https://dl.acm.org/citation.cfm?doid=3176245.3167084),
  [the Parity Light Protocol](https://wiki.parity.io/The-Parity-Light-Protocol-%28PIP%29)
  (PR for similar light client functionality [in geth](https://github.com/ethereum/go-ethereum/pull/16534/files)),
  [Consensys Labs' Ethereum developer tools list](https://github.com/ConsenSysLabs/ethereum-developer-tools-list),
  [Solium, linter and formatter for Solidity](https://github.com/duaraghav8/solium),
  [Ethertrust, analysis tool for smart contracts](https://www.netidee.at/ethertrust).

* Compiling to the EVM:
  [Pirapira's Bamboo](https://github.com/pirapira/bamboo).

* Formalization for smart contracts:
  [Scilla-coq](https://github.com/ilyasergey/scilla-coq)
  (see paper [Scilla: a Smart Contract Intermediate-Level LAnguage](http://ilyasergey.net/papers/scilla-overview.pdf)),
  [TezosCoq](https://github.com/tezos/tezoscoq),
  [Ergo](https://ergo.readthedocs.io/en/latest/Overview.html),
  [Russell O'Connor: "Simplicity: A New Language for Blockchains"](https://arxiv.org/abs/1711.03028).

* Atomic swaps:
  [Ethereum atomic swaps with Bitcoin and most other coins, about to hit release](https://www.reddit.com/r/ethereum/comments/865e0l/ethereum_atomic_swaps_with_bitcoin_and_most_other/),
  [AltCoin](https://github.com/AltCoinExchange/ethatomicswap/),
  [RepublicProtocol](https://github.com/republicprotocol/eth-atomic-swap),
  [Komodo BarterDEX](https://komodoplatform.com/decentralized-exchange/)

* Building secure software:
  [Some thoughts on security after ten years of qmail 1.0 (2007)](https://cr.yp.to/qmail/qmailsec-20071101.pdf),
  ...
