open Ctypes
open Foreign

module Sma_foreign = struct
  let next =
    foreign "sma_next"
      ( ptr double @-> ptr double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let last =
    foreign "sma_last"
      ( ptr double @-> ptr double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let linear =
    foreign "sma_linear"
      ( ptr double @-> ptr double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )
end
