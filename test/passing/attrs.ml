type ctx = unit

type t = {
  name : string; [@resolver fun _ t -> "Name: " ^ t.name]
  age : int; [@doc "An integer to represent the age"]
}
[@@deriving graphql]

type enum = Hello [@doc "Hello"] | World [@doc "World"] [@@deriving graphql]
