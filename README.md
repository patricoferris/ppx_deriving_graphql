ppx_deriving_graphql
--------------------

This ppx currently generates simple `Graphql_lwt.Schema.typ` and `Graphql_lwt.Schema.Arg.arg_typ` values for some OCaml types. There is limited support for adding documentation to your schema's. There are two GraphQL-specific attributes provided by this ppx.

 - `[@resolver (fun ctx p -> p.field_name)]` -- when producing a schema for a record, the `Graphql_lwt.Schema.obj` function expects fields to provide a resolver that uses the context and the record itself to produce a field. By default `ppx_deriving_graphql` will use `fun _ p -> p.field_name` which is fairly common. You can override this with the `[@resolver ]` annotation.
 - `[@doc "Some documentation"]` will add the documentation comment.

In the future this ppx would also like to parse GraphQL SDL's and generate the corresponding values for them too. See the corresponding issue: https://github.com/andreas/ocaml-graphql-server/issues/185