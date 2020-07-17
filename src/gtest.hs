type MuSecX = Int
toSec :: MuSecX -> SecX
toSec m = div m 1000000
toMin :: MuSecX -> MinX
toMin m = div m 60000000

type SecX = Int
toMuSec :: SecX -> MuSecX
toMuSec = (1000000 *)
toMin :: SecX -> MinX
toMin m = div m 60

type MinX = Int
toMuSec :: MinX -> SecX
toMuSec = (60000000 *)
toSec :: MinX -> SecX
toSec = (60 *)

main: do
  let x = 20 :: MinX
  let y = 20 :: SecX
  let z = x + y
