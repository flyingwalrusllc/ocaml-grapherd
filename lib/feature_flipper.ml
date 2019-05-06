open Core

type t = {name: string; percent: float; id: int}

let create name percent id = {name; percent; id}

let update feature percent =
  {name= feature.name; percent; id= feature.id}
  
let feature feature =
  feature.percent >= Random.float 1.0

let%test "features" =
  let featureA = create "featureA" 0.33 1 in
  let rec testy count winning_throws =
    let wins = if feature featureA then winning_throws + 1 else winning_throws in
    let z = count - 1 in
    if z > 0 then (testy z wins) else wins in
  let winners = testy 1000 0 in
  let _ = Printf.printf "got %d winners" winners in
  (winners >= 300) && (winners <= 350)
