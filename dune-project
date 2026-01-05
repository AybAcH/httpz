(lang dune 3.20)
(name httpl)

(package
 (name httpl)
 (synopsis "Zero-copy HTTP/1.1 parser and serializer for OCaml")
 (description "A high-performance HTTP/1.1 parser and serializer using zero-copy multi-buffer spans")
 (depends
  (ocaml (>= 5.0))
  magic-mime))
