(** Parse one hex char into an integer *)
val int_of_hex_char : char -> int

(** Given a string, a start position, and an index i, considering the substring
    from the start position as a big endian number in base 256, return that
    number's ith hexadecimal digit. *)
val hex_digit_of_string : string -> int -> int -> int

(** Parse one character in a string as a hex digit, returning its 4-bit nibble value *)
val parse_hex_nibble : string -> int -> int

(** Parse two character in a string as a two-hex-digit number, returning the 8-bit byte value *)
val parse_hex_byte : string -> int -> int

(** Parse a substring of hex digits starting at position pos and of length len
    as a string of 8-bit characters represented by those digits.
    If the length is odd, then the first character is represented by a single digit.
*)
val parse_hex_substring : string -> int -> int -> string

(** Parse a string of hex digits as a string of 8-bit characters represented by those digits.
    If the length is odd, then the first character is represented by a single digit.
*)
val parse_hex_string : string -> string

(** Parse a hex string of form "nn:nn:...:nn", where nn represents a char as a hex-digit pair *)
val parse_coloned_hex_string : string -> string

(** Unparse a 4-bit digit into a hex character *)
val hex_char_of_int : ?upper_case:bool -> int -> char

(** Unparse a substring of string starting at position pos and with length len as
    a string of an even number of hex characters.
*)
val unparse_hex_substring : string -> int -> int -> string

(** Unparse a string as a string of an even number of hex characters. *)
val unparse_hex_string : string -> string

(** Unparse a string as a string "nn:nn:...:nn", where nn represents a char as a hex-digit pair *)
val unparse_coloned_hex_string : string -> string

(** raise an Internal_error if the string doesn't strictly start with "0x" *)
val validate_0x_prefix : string -> unit

(** first check that a string strictly starts with "0x" then call parser on the rest of the string *)
val parse_0x_prefix : (string -> 'a) -> string -> 'a

(** create a string that starts with "0x" then the result of unparsing the given data *)
val unparse_0x_prefix : ('a -> string) -> 'a -> string

(** parse a string as a 0x-prefixed hex string *)
val parse_0x_string : string -> string

(** unparse a string as a 0x-prefixed hex string *)
val unparse_0x_string : string -> string
