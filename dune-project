(lang dune 1.10)
(name grapherd)
(using fmt 1.1)
(generate_opam_files true)
(license GPL-3.0-or-later)
(authors "Chris Vale")
(maintainers "crispywalrus@gmail.com")
(source (github flyingwalrusllc/ocaml-grapherd))

(package
 (name libgraph)
 (synopsis "Library for a simple high performance graph server")
 (depends
  (core (> 0.12))
  (ppx_deriving (> 4.2))
  (higher (> 0.3))
  (psq (> 0.2))
  (dune (and :build (> 1.10)))
  (bisect_ppx (> 1.4))
  (crunch (and :build))))

(package
 (name grapherd)
 (synopsis "RPC capable server linked with libgraph")
 (depends
  (core (> 0.12))
  (ppx_deriving (> 4.2))
  (ppx_jane (> 0.12))
  (higher (> 0.3))
  (psq (> 0.2))
  (dune (and :build (> 1.10)))
  (crunch (and :build))))
