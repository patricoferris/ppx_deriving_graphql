ppx_deriving_graphql
--------------------

*Status: WIP and Experimental*

This ppx currently generates simple `Graphql_lwt.Schema.typ` and `Graphql_lwt.Schema.Arg.arg_typ` values for some OCaml types. There is limited support for adding documentation to your schema's. There are two GraphQL-specific attributes provided by this ppx.

<!-- TOC -->

- [ppx_deriving_graphql](#ppx_deriving_graphql)
- [Usage](#usage)
    - [[@@deriving graphql]](#deriving-graphql)
    - [Running Queries](#running-queries)
    - [Attributes](#attributes)
        - [[@resolver]](#resolver)
        - [[@doc]](#doc)
- [Future Work](#future-work)

<!-- /TOC -->

## Usage

What follows is a basic tutorial on how to use the ppx.

### [@@deriving graphql]

The simplest use of the ppx is to generate both a schema and an argument. If your type is called `person` then two functions are generated called `person_schema_typ` and `person_arg_typ`.

```ocaml
# type person = { name : string }[@@deriving graphql];;
Line 1, characters 1-52:
Error: Unbound type constructor ctx
```

Schemas require a context. Currently, this ppx assumes that that GraphQL context is available within the file as `ctx`. In the future it might make sense to make this configurable.

```ocaml
type ctx = unit
type person = { name : string }[@@deriving graphql]
```

With the context defined, we can then use these values that have been generated.

```ocaml
# person_schema_typ;;
- : (ctx, person option) Graphql_lwt.Schema.typ = <abstr>
# person_arg_typ;;
- : person option Graphql_lwt.Schema.Arg.arg_typ = <abstr>
```

As with many other ppx derivers, the prefix using the type constructor name is dropped if the type is called `t`.

```ocaml
# type t = string [@@deriving graphql];;
type t = string
val arg_typ : t option Graphql_lwt.Schema.Arg.arg_typ = <abstr>
val schema_typ : (ctx, t option) Graphql_lwt.Schema.typ = <abstr>
```

### Running Queries

The [prelude file](./doc/prelude.txt) contains a simple `make_schema` function which makes a new field called `"field"` with a supplied type and set of data. We first need to construct a valid query and some fake data to use.

```ocaml
let query = Graphql_parser.parse {| { field { name } } |} |> Result.get_ok
let data = { name = "Alice" }
```

We can the execute the query on the data. We also pass in the `()` value for our context.

```ocaml
# Graphql_lwt.Schema.(execute (make_schema (non_null person_schema_typ) data) () query);;
- : [ `Response of Yojson.Basic.t
    | `Stream of
        Yojson.Basic.t Graphql_lwt.Schema.response
        Graphql_lwt.Schema.Io.Stream.t ]
    Graphql_lwt.Schema.response
=
Ok
 (`Response
    (`Assoc
       [("data", `Assoc [("field", `Assoc [("name", `String "Alice")])])]))
```

### Attributes

There are various attributes that offer you more control and customisation over the implementation of the schemas and arguments.

#### [@resolver]

The resolver attribute allows you to override how schema objects resolve fields. By default the ppx will use `fun _ p -> p.<field_name>` which is fairly common. You can override this with the `[@resolver ]` annotation.

```ocaml
type t = { 
    name : string [@resolver fun _ p -> ("Name: " ^ p.name)] 
}[@@deriving graphql]
let data = { name = "Alice" }
let query = Graphql_parser.parse {| { field { name } } |} |> Result.get_ok
```

We can how this changes the resolver by executing a query.

```ocaml
# Graphql_lwt.Schema.(execute (make_schema (non_null schema_typ) data) () query);;
- : [ `Response of Yojson.Basic.t
    | `Stream of
        Yojson.Basic.t Graphql_lwt.Schema.response
        Graphql_lwt.Schema.Io.Stream.t ]
    Graphql_lwt.Schema.response
=
Ok
 (`Response
    (`Assoc
       [("data",
         `Assoc [("field", `Assoc [("name", `String "Name: Alice")])])]))
```

#### [@doc] 

`[@doc "Some documentation"]` will add the documentation comment for a particular field or variant constructor which is converted to an enum.

## Future Work

In the future this ppx would also like to parse GraphQL SDL's and generate the corresponding values for them too. See the corresponding issue: https://github.com/andreas/ocaml-graphql-server/issues/185
