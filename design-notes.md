# The yojson framework

Let's consider `Revision.of_yojson_exn`. Merlin reports its type as `Revision.t
of_yojson_exn`. All of this occurs in the library `Legilogic_lib`. What's going
on here is that `Types.Revision` is a `DbInt`, and that includes
`Persisting.PersistableS`, which includes `Yojsoning.YojsonableS`, which
contains a *value* labeled `of_yojson_exn`, which is also of *type*
`of_yojson_exn` (also defined in `Persisting`). That type is a function, `yojson
-> 'a`.

# Persistence and messages

Rule 1: NEVER, EVER send any effectful (as opposed to informative) message to anyone else (including the blockchain, but not limited to it), until you've committed the fact that you're sending the message.

Rule 2: EVERY SINGLE effectful message whatsoever MUST be idempotent.
