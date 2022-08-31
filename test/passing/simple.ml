type ctx
type x = int [@@deriving graphql]
type v = { name : string option; books : string list } [@@deriving graphql]

module T : sig
  type inner [@@deriving graphql]
end = struct
  type inner = { name : string } [@@deriving graphql]
end

type p = T.inner [@@deriving graphql]

module A = struct
  type train_class = [ `Standard | `First_class ] [@@deriving graphql]

  type train_details = {
    origin : string;
    destination : string;
    via : string list;
    train_class : train_class;
    train_type : string option;
    number_of_people : int;
  }
  [@@deriving graphql]
end

module B = struct
  type train_class = A.train_class [@@deriving graphql]

  type train_details = A.train_details = {
    origin : string;
    destination : string;
    via : string list;
    train_class : train_class;
    train_type : string option;
    number_of_people : int;
  }
  [@@deriving graphql]
end

(* Only generate schema's *)

module S : sig
  type person [@@deriving graphql_schema]
end = struct
  type person = {
    name : string; [@doc "A person's name"]
    age : int; [@doc "A person's age"]
  }[@@deriving graphql_schema]
end