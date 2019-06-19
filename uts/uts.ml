module BArray = Bigarray

type float64_bigarray =
  (float, BArray.float64_elt, BArray.c_layout) BArray.Array1.t

type uts_t = {values: float64_bigarray}
