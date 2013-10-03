-- adler32 checksum (fold left)
import Control.Arrow ((***))
import Control.Monad (join)
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base = 65521

adler32foldl xs = let(mysum, interim) = foldl step (1, 0) xs
                    in (interim `shiftL` 16) .|. mysum
            where step (mysum, interim) x = let mysum' = mysum + (ord x .&. 0xff)
                                                        in (mysum' `mod` base, (mysum' + interim) `mod` base)