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
  (base (> 0.12))
  (ppx_inline_test (> 0.12))
  (ppx_jane (> 0.12))
  (ppx_deriving (> 4.2))
  (higher (> 0.3))
  (psq (> 0.2))
  (dune (and :build (> 1.10)))
  (bisect_ppx (> 1.4))
  (crunch (and :build))))

(package
 (name uts)
 (synopsis "Algorithms for Unevenly Spaced Time Series c stubs"))

(package
 (name utils)
 (synopsis "Some simple server utilities")
 (depends
  (base (> 0.12))
  (ppx_inline_test (> 0.12))
  (ppx_jane (> 0.12))
  (ppx_deriving (> 4.2))
  (dune (and :build (> 1.10)))
  (bisect_ppx (> 1.4))
  (crunch (and :build))))

(package
 (name grapherd)
 (synopsis "RPC capable server linked with libgraph")
 (depends
  (base (> 0.12))
  (ppx_deriving (> 4.2))
  (ppx_jane (> 0.12))
  httpaf
  httpaf-async
  async
  mysql
  higher
  (ppx_mysql (>= 1.0))
  (bisect_ppx (> 1.4))
  (dune (and :build (> 1.10)))
  (crunch (and :build))))

 ;; Local Variables:
 ;; mode: dune
 ;; End:

