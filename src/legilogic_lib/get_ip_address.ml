open Unix

let ip_address_regexp = Str.regexp "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$"

(** True if [s] is an ip address quartet, false otherwise *)
let is_ip_address (s : string) : bool = Str.string_match ip_address_regexp s 0

(** Return the address associated with an IP address as a string, or a hostname *)
let inet_addr_from_ip_or_host (host : string) : inet_addr =
  if is_ip_address host
  then inet_addr_of_string host
  else (gethostbyname host).h_addr_list.(0)

module Test = struct
  let _ =
    assert (is_ip_address "127.0.0.1");
    assert (not @@ is_ip_address "harry! 127.0.0.1");
    assert (inet_addr_from_ip_or_host "127.0.0.1" =
              inet_addr_from_ip_or_host "localhost")
end
