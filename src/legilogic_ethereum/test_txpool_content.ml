(* https://github.com/ethereum/go-ethereum/wiki/Management-APIs/c371c57e#example-14 *)
let example_valid_response = {|
{
  "pending": {
    "0x0216d5032f356960cd3749c31ab34eeff21b3395": {
      "806": [
        {
          "blockHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
          "blockNumber": null,
          "from": "0x0216d5032f356960cd3749c31ab34eeff21b3395",
          "gas": "0x5208",
          "gasPrice": "0xba43b7400",
          "hash": "0xaf953a2d01f55cfe080c0c94150a60105e8ac3d51153058a1f03dd239dd08586",
          "input": "0x",
          "nonce": "0x326",
          "to": "0x7f69a91a3cf4be60020fb58b893b7cbb65376db8",
          "transactionIndex": null,
          "value": "0x19a99f0cf456000"
        }
      ]
    },
    "0x24d407e5a0b506e1cb2fae163100b5de01f5193c": {
      "34": [
        {
          "blockHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
          "blockNumber": null,
          "from": "0x24d407e5a0b506e1cb2fae163100b5de01f5193c",
          "gas": "0x44c72",
          "gasPrice": "0x4a817c800",
          "hash": "0xb5b8b853af32226755a65ba0602f7ed0e8be2211516153b75e9ed640a7d359fe",
          "input": "0xb61d27f600000000000000000000000024d407e5a0b506e1cb2fae163100b5de01f5193c00000000000000000000000000000000000000000000000053444835ec580000000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
          "nonce": "0x22",
          "to": "0x7320785200f74861b69c49e4ab32399a71b34f1a",
          "transactionIndex": null,
          "value": "0x0"
        }
      ]
    }
  },
  "queued": {
    "0x976a3fc5d6f7d259ebfb4cc2ae75115475e9867c": {
      "3": [
        {
          "blockHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
          "blockNumber": null,
          "from": "0x976a3fc5d6f7d259ebfb4cc2ae75115475e9867c",
          "gas": "0x15f90",
          "gasPrice": "0x4a817c800",
          "hash": "0x57b30c59fc39a50e1cba90e3099286dfa5aaf60294a629240b5bbec6e2e66576",
          "input": "0x",
          "nonce": "0x3",
          "to": "0x346fb27de7e7370008f5da379f74dd49f5f2f80f",
          "transactionIndex": null,
          "value": "0x1f161421c8e0000"
        }
      ]
    },
    "0x9b11bf0459b0c4b2f87f8cebca4cfc26f294b63a": {
      "2": [
        {
          "blockHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
          "blockNumber": null,
          "from": "0x9b11bf0459b0c4b2f87f8cebca4cfc26f294b63a",
          "gas": "0x15f90",
          "gasPrice": "0xba43b7400",
          "hash": "0x3a3c0698552eec2455ed3190eac3996feccc806970a4a056106deaf6ceb1e5e3",
          "input": "0x",
          "nonce": "0x2",
          "to": "0x24a461f25ee6a318bdef7f33de634a67bb67ac9d",
          "transactionIndex": null,
          "value": "0xebec21ee1da40000"
        }
      ],
      "6": [
        {
          "blockHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
          "blockNumber": null,
          "from": "0x9b11bf0459b0c4b2f87f8cebca4cfc26f294b63a",
          "gas": "0x15f90",
          "gasPrice": "0x4a817c800",
          "hash": "0xbbcd1e45eae3b859203a04be7d6e1d7b03b222ec1d66dfcc8011dd39794b147e",
          "input": "0x",
          "nonce": "0x6",
          "to": "0x6368f3f8c2b42435d6c136757382e4a59436a681",
          "transactionIndex": null,
          "value": "0xf9a951af55470000"
        },
        {
          "blockHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
          "blockNumber": null,
          "from": "0x9b11bf0459b0c4b2f87f8cebca4cfc26f294b63a",
          "gas": "0x15f90",
          "gasPrice": "0x4a817c800",
          "hash": "0x60803251d43f072904dc3a2d6a084701cd35b4985790baaf8a8f76696041b272",
          "input": "0x",
          "nonce": "0x6",
          "to": "0x8db7b4e0ecb095fbd01dffa62010801296a9ac78",
          "transactionIndex": null,
          "value": "0xebe866f5f0a06000"
        }
      ]
    }
  }
}
|}

let example_invalid_response_null_block_hash = {|
{
  "pending": {
    "0x0216d5032f356960cd3749c31ab34eeff21b3395": {
      "806": [
        {
          "blockHash": null,
          "blockNumber": null,
          "from": "0x0216d5032f356960cd3749c31ab34eeff21b3395",
          "gas": "0x5208",
          "gasPrice": "0xba43b7400",
          "hash": "0xaf953a2d01f55cfe080c0c94150a60105e8ac3d51153058a1f03dd239dd08586",
          "input": "0x",
          "nonce": "0x326",
          "to": "0x7f69a91a3cf4be60020fb58b893b7cbb65376db8",
          "transactionIndex": null,
          "value": "0x19a99f0cf456000"
        }
      ]
    }
  },
  "queued": {}
  }
|}

let example_invalid_response_malformed_nonces = {|
{ "pending": {}
, "queued":  { "0x976a3fc5d6f7d259ebfb4cc2ae75115475e9867c": [{ "foo": null }]}
}
|}

let example_invalid_response_malformed_nonces_list = {|
{ "pending": {}
, "queued":  { "0x976a3fc5d6f7d259ebfb4cc2ae75115475e9867c": { "3": { "foo": null }}}
}
|}
