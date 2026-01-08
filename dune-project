(lang dune 3.20)
(name domainpc)
(version dev)

(generate_opam_files true)

(license AGPL-3.0-or-later)

(source
 (github hra687261/domainpc))

(authors "<hra@ocamlpro.com>")

(maintainers "<hra@ocamlpro.com>")

(package
 (name domainpc)
 (synopsis "domainpc")
 (description "Domain Per Core, ensure that each domain runs on its own physical core.")
 (depends ocaml processor)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
