-- adler32 checksum (helper)
import Control.Arrow ((***))
import Control.Monad (join)
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base = 65521

adler32pair xs = helper (1,0) xs
    where helper (mysum,interim) (x:xs) =
                let mysum' = (mysum + (ord x .&. 0xff)) `mod` base
                    interim' = (mysum' + interim) `mod` base
                in helper (mysum',interim') xs
          helper (mysum,interim) _    = (interim `shiftL` 16) .|. mysum
