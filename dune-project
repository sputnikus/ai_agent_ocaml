(lang dune 3.19)

(name camelgent)

(generate_opam_files true)

(source
 (github sputnikus/camelgent))

(authors "Martin Putniorz <mputniorz@gmail.com>")

(maintainers "Martin Putniorz <mputniorz@gmail.com>")

(license LICENSE)

(package
 (allow_empty)
 (name camelgent)
 (synopsis "Coding AI Agent implemented in OCaml")
 (depends ocaml lwt lwt_ppx cohttp-lwt-unix yojson)
 (tags
  ("LLM" "AI Agent")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
