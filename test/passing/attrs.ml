type ctx = unit
type t =
  {
  name: string [@resolver fun _ -> fun t -> "Name: " ^ t.name];
  age: int [@doc "An integer to represent the age"]}[@@deriving graphql]
include
  struct
    let (arg_typ : t option Graphql_lwt.Schema.Arg.arg_typ) =
      Graphql_lwt.Schema.Arg.obj "TInput"
        ~fields:[Graphql_lwt.Schema.Arg.arg
                   ?doc:(Some "An integer to represent the age") "age"
                   ~typ:(Graphql_lwt.Schema.Arg.non_null
                           Graphql_lwt.Schema.Arg.int);
                Graphql_lwt.Schema.Arg.arg ?doc:None "name"
                  ~typ:(Graphql_lwt.Schema.Arg.non_null
                          Graphql_lwt.Schema.Arg.string)]
        ~coerce:(fun age -> fun name -> { name; age })
    let (schema_typ : (ctx, _) Graphql_lwt.Schema.typ) =
      Graphql_lwt.Schema.obj "T"
        ~fields:[Graphql_lwt.Schema.field
                   ?doc:(Some "An integer to represent the age") "age"
                   ~typ:(Graphql_lwt.Schema.non_null Graphql_lwt.Schema.int)
                   ~args:[] ~resolve:(fun _ -> fun p -> p.age);
                Graphql_lwt.Schema.field ?doc:None "name"
                  ~typ:(Graphql_lwt.Schema.non_null Graphql_lwt.Schema.string)
                  ~args:[] ~resolve:(fun _ -> fun t -> "Name: " ^ t.name)]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type enum =
  | Hello [@doc "Hello"]
  | World [@doc "World"][@@deriving graphql]
include
  struct
    let (enum_arg_typ : enum option Graphql_lwt.Schema.Arg.arg_typ) =
      Graphql_lwt.Schema.Arg.enum "EnumInput"
        ~values:[Graphql_lwt.Schema.enum_value "World" ?doc:(Some "World")
                   ~value:World;
                Graphql_lwt.Schema.enum_value "Hello" ?doc:(Some "Hello")
                  ~value:Hello]
    let (enum_schema_typ : (ctx, _) Graphql_lwt.Schema.typ) =
      Graphql_lwt.Schema.enum "Enum"
        ~values:[Graphql_lwt.Schema.enum_value "World" ~doc:(Some "World")
                   ~value:World;
                Graphql_lwt.Schema.enum_value "Hello" ~doc:(Some "Hello")
                  ~value:Hello]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
