open Base

type t = {name: string; percent: float; id: int; description: string}

let create name percent id description = {name; percent; id; description}

let update feature percent =
  { name= feature.name
  ; percent
  ; id= feature.id
  ; description= feature.description }

let feature feature =
  let roll = Random.float 1. in
  Float.compare feature.percent roll >= 0

let%test "featureA" =
  let featureA = create "featureA" 0.33 1 "test feature A" in
  let rec testy count winning_throws =
    let wins =
      if feature featureA then winning_throws + 1 else winning_throws
    in
    let z = count - 1 in
    if z > 0 then testy z wins else wins
  in
  let winners = testy 1000 0 in
  winners >= 300 && winners <= 350
