open Ctypes
open Foreign

module Ema_foreign = struct
  let next =
    foreign "ema_next"
      ( ptr double @-> ptr double @-> ptr int @-> ptr double @-> ptr double
      @-> returning void )

  let last =
    foreign "ema_last"
      ( ptr double @-> ptr double @-> ptr int @-> ptr double @-> ptr double
      @-> returning void )

  let linear =
    foreign "ema_linear"
      ( ptr double @-> ptr double @-> ptr int @-> ptr double @-> ptr double
      @-> returning void )
end
