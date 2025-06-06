(lang dune 3.18)
(using menhir 3.0)

(name open_lambda)

(generate_opam_files true)

(source
 (github bloomwayz/open-lambda))

(authors "Junyoung Park <bloomwayz@snu.ac.kr>")

(maintainers "Junyoung Park <bloomwayz@snu.ac.kr>")

(package
 (name open_lambda)
 (synopsis "A simple multi-staged language")
 (description "A simple multi-staged language")
 (depends
   ocaml
   (ocaml-base-compiler
     (= 5.3.0))
   menhir)
 (tags
  ("Programming Language")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
