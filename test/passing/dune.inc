; -------- Test: `attrs.ml` --------

; The PPX-dependent executable under test
(executable
 (name attrs)
 (modules attrs)
 (preprocess (pps ppx_deriving_graphql))
 (libraries graphql-lwt))

; Run the PPX on the `.ml` file
(rule
 (targets attrs.actual)
 (deps
  (:pp pp.exe)
  (:input attrs.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_deriving_graphql)
 (action
  (diff attrs.expected attrs.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_deriving_graphql)
 (action
  (run ./attrs.exe)))

; -------- Test: `simple.ml` --------

; The PPX-dependent executable under test
(executable
 (name simple)
 (modules simple)
 (preprocess (pps ppx_deriving_graphql))
 (libraries graphql-lwt))

; Run the PPX on the `.ml` file
(rule
 (targets simple.actual)
 (deps
  (:pp pp.exe)
  (:input simple.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_deriving_graphql)
 (action
  (diff simple.expected simple.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_deriving_graphql)
 (action
  (run ./simple.exe)))


