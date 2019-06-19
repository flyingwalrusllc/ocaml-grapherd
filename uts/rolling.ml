open Ctypes
open Foreign

module Rolling_foreign = struct
  let central_moment =
    foreign "rolling_central_moment"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> ptr double @-> returning void )

  let max =
    foreign "rolling_max"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let mean =
    foreign "rolling_mean"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let median =
    foreign "rolling_median"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let min =
    foreign "rolling_min"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let num_obs =
    foreign "rolling_num_obs"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let product =
    foreign "rolling_product"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let sd =
    foreign "rolling_sd"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let sum =
    foreign "rolling_sum"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let sum_stable =
    foreign "rolling_sum_stable"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )

  let var =
    foreign "rolling_var"
      ( ptr double @-> double @-> ptr int @-> ptr double @-> ptr double
      @-> ptr double @-> returning void )
end
