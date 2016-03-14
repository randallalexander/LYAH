import System.Random
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

-- threeCoins (mkStdGen 22)
-- take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

-- B.pack [98..120]
-- B.cons 85 $ B.pack [80,81,82,84] --lazy
-- B.cons' 85 $ B.pack [80,81,82,84] --strict