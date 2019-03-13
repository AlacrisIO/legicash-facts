
type 'a to_rlp_item       = 'a -> Rlp.rlp_item
type 'a of_rlp_item       = Rlp.rlp_item -> 'a
type 'a of_rlp_item_opt   = Rlp.rlp_item -> 'a option

type 'a to_rlp            = 'a -> string
type 'a of_rlp            = string -> 'a
type 'a of_rlp_opt        = string -> 'a option

type 'a marshal_rlp       = Buffer.t -> 'a -> unit
type 'a unmarshal_rlp     = int -> string -> ('a * int)
type 'a unmarshal_rlp_opt = int -> string -> ('a * int) option

type 'a pre_rlping = { to_rlp_item : 'a to_rlp_item;
                       of_rlp_item : 'a of_rlp_item }

type 'a rlping = { to_rlp_item       : 'a to_rlp_item;
                   of_rlp_item       : 'a of_rlp_item;
                   of_rlp_item_opt   : 'a of_rlp_item_opt;
                   to_rlp            : 'a to_rlp;
                   of_rlp            : 'a of_rlp;
                   of_rlp_opt        : 'a of_rlp_opt;
                   marshal_rlp       : 'a marshal_rlp;
                   unmarshal_rlp     : 'a unmarshal_rlp;
                   unmarshal_rlp_opt : 'a unmarshal_rlp_opt }

