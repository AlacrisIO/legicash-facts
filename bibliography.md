# Bibliography

"You can save hours reading papers with months of hard work on your project."

* [Plasma](https://plasma.io/),
  [Plasma Group](https://plasma.group/),
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

* OCaml to JavaScript: BuckleScript, ReasonML.
  [nix-shell for BuckleScript](https://github.com/reazen/relude/blob/master/default.nix),
  [tablecloth, An ergonomic, cross-platform, standard library for ReasonML and OCaml](https://github.com/darklang/tablecloth)

* Purely Functional Data Structures:
 [Okasaki's book](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf),
 [a follow-up](https://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki)

* [Computability Logic](http://www.csc.villanova.edu/~japaridz/CL/)

* [K Framework](http://www.kframework.org/index.php/Main_Page),
  [KEVM: Semantics of EVM in K](https://github.com/kframework/evm-semantics),
  and the [Jello Paper](https://jellopaper.org/)

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
   [Conversational Concurrency](http://syndicate-lang.org/tonyg-dissertation/) by Tony Garnock-Jones.

* Byzantine Fault Tolerance:
 [Papers selected by Rick Dudley](https://medium.com/@AFDudley/byzantine-fault-tolerant-consensus-papers-1b4b47d27463) (2015);
 [A Guide to 99% Fault Tolerant Consensus, by Vitalik Buterin](https://vitalik.ca/general/2018/08/07/99_fault_tolerant.html).

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
  [Zeppelin: deconstructing a solidity contract](https://blog.zeppelin.solutions/deconstructing-a-solidity-contract-part-ii-creation-vs-runtime-6b9d60ecb44c),
  [Proxy libraries](https://blog.zeppelin.solutions/proxy-libraries-in-solidity-79fbe4b970fd),
  [Security Audit by QuantStamp](https://quantstamp.com/start),
  [Towards verifying ethereum smart contract bytecode in Isabelle/HOL](https://dl.acm.org/citation.cfm?doid=3176245.3167084),
  [the Parity Light Protocol](https://wiki.parity.io/The-Parity-Light-Protocol-%28PIP%29)
  (PR for similar light client functionality [in geth](https://github.com/ethereum/go-ethereum/pull/16534/files)),
  [Consensys Labs' Ethereum developer tools list](https://github.com/ConsenSysLabs/ethereum-developer-tools-list),
  [Solium, linter and formatter for Solidity](https://github.com/duaraghav8/solium),
  [Ethertrust, analysis tool for smart contracts](https://www.netidee.at/ethertrust),
  [The Challenges of Building Ethereum Infrastructure](https://medium.com/@lopp/the-challenges-of-building-ethereum-infrastructure-87e443e47a4b).

* Compiling to the EVM:
  [pirapira's efforts](https://github.com/pirapira/ethereum-formal-verification-overview/blob/master/README.md) including
  [eth-isabelle](https://github.com/pirapira/eth-isabelle) and
  [Bamboo](https://github.com/cornellblockchain/bamboo),
  [EtherVM](https://ethervm.io/),
  [Yellow Paper](https://ethereum.github.io/yellowpaper/paper.pdf),
  [Jello Paper](https://jellopaper.org/evm/).

* Formal Methods:
  [What do Formal Methods actually Guarantee?](https://medium.com/alacris/what-do-formal-methods-actually-guarantee-d94ae8802be2)
  [Formally Verified Software in the Real World](https://cacm.acm.org/magazines/2018/10/231372-formally-verified-software-in-the-real-world/fulltext),
  [Z3 tutorial](https://rise4fun.com/z3/tutorial).

* Formalization for smart contracts:
  [Scilla-coq](https://github.com/ilyasergey/scilla-coq)
  (see paper [Scilla: a Smart Contract Intermediate-Level LAnguage](http://ilyasergey.net/papers/scilla-overview.pdf)),
  [TezosCoq](https://github.com/tezos/tezoscoq),
  [Ergo](https://ergo.readthedocs.io/en/latest/Overview.html),
  [Russell O'Connor: "Simplicity: A New Language for Blockchains"](https://arxiv.org/abs/1711.03028),
  [Peng Wang](https://people.csail.mit.edu/wangpeng/)['s](https://www.csail.mit.edu/event/type-system-resource-bounds-type-preserving-compilation-and-its-application-ethereum-smart) [thesis](https://people.csail.mit.edu/wangpeng/phd-thesis.pdf) ([code](https://github.com/wangpengmit/phd-thesis-supplemental), [timl](https://github.com/mit-plv/timl)),
  [ERC777-K](https://runtimeverification.com/blog/erc777-k-formal-executable-specification-of-erc777/).

* Atomic swaps:
  [Ethereum atomic swaps with Bitcoin and most other coins, about to hit release](https://www.reddit.com/r/ethereum/comments/865e0l/ethereum_atomic_swaps_with_bitcoin_and_most_other/),
  [AltCoin](https://github.com/AltCoinExchange/ethatomicswap/),
  [RepublicProtocol](https://github.com/republicprotocol/eth-atomic-swap),
  [Komodo BarterDEX](https://komodoplatform.com/decentralized-exchange/)

* Building secure software:
  [Some thoughts on security after ten years of qmail 1.0 (2007)](https://cr.yp.to/qmail/qmailsec-20071101.pdf),
  ...

* UI tools for Crypto-currency dApps:
  [MetaMask Browser Extension](https://github.com/MetaMask/metamask-extension)

* Usability and UX:
  [Nielsen Norman Group](https://www.nngroup.com/articles/)

* Discipline for more robust programming
  [STAMPING ON EVENT-STREAM](https://www.hillelwayne.com/post/stamping-on-eventstream/)

* Low-level Cryptographic protocols:
  [Threshold-optimal DSA/ECDSA signatures and an application to Bitcoin wallet security](https://eprint.iacr.org/2016/013.pdf)

* Interactive help for Ethereum:
  [Go Ethereum gitter](https://gitter.im/ethereum/go-ethereum),
  [Ethereum Magicians](https://ethereum-magicians.org/top/all)...

* Nix: [Overlays](https://nixos.org/nixpkgs/manual/#chap-overlays),
  [Typing Nix](https://www.tweag.io/posts/2017-05-23-typing-nix.html)
  ([tix-papers](https://github.com/regnat/tix-papers), [tix](https://github.com/regnat/tix))...

* Formal Method advocacy:
  [Formally Verified Software in the Real World](https://cacm.acm.org/magazines/2018/10/231372-formally-verified-software-in-the-real-world/fulltext),
  [What do Formal Methods actually Guarantee?](https://medium.com/alacris/what-do-formal-methods-actually-guarantee-d94ae8802be2)

* Homomorphic encryption:
  [Reusable Non-Interactive Secure Computation](https://eprint.iacr.org/2018/940.pdf),
  [Efficent Multi-Party computation toolkit](https://github.com/emp-toolkit),
  [Jonathan Katz](http://www.cs.umd.edu/~jkatz/papers.html),
  ... something in F* or Coq from MSR Cambridge?

* Algorand:
  See the original whitepaper,
  [Vault](https://eprint.iacr.org/2018/269.pdf),
  and their bibliography...
  also [flaws?](https://hackernoon.com/a-fatal-flaw-in-algorand-professor-yongge-wang-takes-apart-their-renown-consensus-agreement-4c111286cdbb)

* More Coq: [Bedrock](https://github.com/mit-plv/bedrock2)

* Contracts on BCH:
  [New Bitcoin Cash Opcode Shows an Onchain Game of Chess is Possible](https://news.bitcoin.com/new-bitcoin-cash-opcode-shows-an-onchain-game-of-chess-is-possible/)

* Other bibliographies:
  [Bitcoin History](https://infominer.id/bitcoin-history/),
  [IOHK papers](https://iohk.io/research/papers/),
  [CoinMetrics resources](https://coinmetrics.io/resources/),
  (some old timer once published a link to a trove of papers... where?),
  ...
