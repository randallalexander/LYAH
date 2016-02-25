import qualified Data.List as List
import Data.Function (on)
import Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube 

intersperseMonkey = List.intersperse '.' "MONKEY"
concatFlatten = List.concat [[3,4,5],[2,3,4],[2,1,1]]
flatmap = List.concatMap (replicate 4) [1..3]

values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
groupBy1 = List.groupBy (\x y -> (x > 0) == (y > 0)) values
groupBy2 = List.groupBy ((==) `on` (> 0)) values

values1 = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
sortByEx = List.sortBy (compare `on` length) values1

digitToInt1 = map Char.digitToInt "FF85AB"
ordA = Char.ord 'a'
chr1 = Char.chr 97

encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map Char.chr shifted  

decode :: Int -> String -> String 
decode shift msg = encode (negate shift) msg

encodeDecode = decode 5 . encode 5 $ "This is a sentence"

phoneBook = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
phoneAdd = Map.insert "foo" "phone" phoneBook
-- phoneGet = Map.lookup "foo"

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!" 
set1 = Set.fromList text1
set2 = Set.fromList text2

sphereVol = Sphere.volume 4.3