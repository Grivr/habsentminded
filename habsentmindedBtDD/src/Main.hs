module Main where

import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad.Error
import Data.ConfigFile (readfile, emptyCP, optionxform, get)
import Data.Bits
import Data.List.Split (split, dropFinalBlank, whenElt, keepDelimsR, chunksOf)
import Data.Word (Word8)
import Data.List (sort, insertBy, elemIndex)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Map                 as M
import qualified Data.Bitstream.Lazy      as Bi
import qualified Data.ByteString.Lazy     as Bs


-----------------------------Encoding part---------------------

data  HuffmanTree a =    Leaf {weight:: Int, val:: a}
                        |Tree {weight:: Int, left:: HuffmanTree a, right:: HuffmanTree a}
                        deriving (Eq)

-- build a multiline string representation of a huffman tree

instance Show a => Show (HuffmanTree a) where
  show = go ""
    where
      paren x = "--" ++ show x -- ++ "+"
      go _ (Leaf _ v) = "-[" ++ show v  ++ "]\n"
      go ss (Tree w l r) =   root ++ go (ss' ++ "|") l
          ++ ss' ++ "|\n" 
          ++ ss' ++ "`" ++ go (ss' ++ " ") r 
          where 
           ss' = ss ++ replicate (length root - 1) ' '
           root = paren w 

-- build a huffman tree bototm-up from a list of symbols sorted by weight
sortedHuffman :: [(Int,a)] -> HuffmanTree a
-- first, convert each tuple into a Leaf, then combine
sortedHuffman = combine . map (uncurry Leaf) 
    where
    -- repeatedly combine lowest weight trees and reinsert the result into the
    -- weight ordered list
    combine [t] = t --got a root tree
    combine (ta: tb: ts) = combine $ insertBy (comparing weight) (mergeTree ta tb) ts
     where
       mergeTree a b = Tree (weight a + weight b) a b
       -- make an internal node from two trees. the weight is the sum 
    


-- traverse the huffman tree generating a map from the symbol to its huffman
-- tree path (where False is left, and True is right). 
codes :: Ord a => HuffmanTree a -> [(a, [Bool])]
codes = go []
  where    
  go p (Leaf _ a) = [(a, reverse p)]-- leaf nodes mark the end of a path to a symbol
  go p (Tree _ l r) = go (False:p) l ++ go (True:p) r

-- from a table mapping symbols to their corresponding huffman tree bit paths,
-- replace each instance of a symbol with its bit path
encode :: Ord a => [(a, [Bool])] -> [a] -> [Bool]
encode tbl = concatMap getTbl
  where
  getTbl x = fromJust (lookup x tbl)


--Get tree breadth first flattened from HuffmanTree and charmap to look for leaf values
--nodes are enumerated ascending, leafs are charmap linked and ORed to 0x80, as
--for BTnDD format
treeLevels :: (Num a, Eq a) => HuffmanTree a -> [a] -> [a]
-- first get list of nodes, 
--then convert to list of words. enumerate nodes and link to charmap entry by the way
treeLevels tree charMap = tail $ nodeVal (levels [tree]) 0 --offset 1 in tree file, start from root, root is not included in trees
  where
  nodeVal [] _  = []
  nodeVal (Tree{} : xs) n  = n : nodeVal xs (n+1) 
  nodeVal (Leaf _ v : xs) n = charMapLink : nodeVal xs n
    where
    charMapLink  = fromIntegral $ fromJust (elemIndex v charMap) + 0x80
  levels [] = []
  levels xs = xs ++ levels (concatMap leftRight xs) 
    where
    leftRight (Leaf _ _) = [] 
    leftRight (Tree _ l r) = [l, r]

--split list into tuple (evenIndexElements, oddIndexElements)
splitOddIndex :: [a] -> ([a],[a])
splitOddIndex [] = ([], [])
splitOddIndex [x] = ([x], [])
splitOddIndex (x:y:xs) = (x:xp, y:yp) where (xp, yp) = splitOddIndex xs

--split list of binary text to blocks of four screens
splitFourScreens:: (Num a, Eq a) => Word8 -> [a] -> [[a]]
splitFourScreens endOfScreen' xs = map concat $ chunksOf 4 (blocks xs)
                  where blocks = split (dropFinalBlank $ keepDelimsR $ whenElt (== fromIntegral endOfScreen')) 

-----------------------------Decoding part---------------------------------

--get decoded plain binary text from input rom file
decodeText :: ConfigInfo -> Bs.ByteString -> [Word8]
decodeText config input= 
  let
    inputU8 =  Bs.unpack input
    treeLeft = map fromIntegral $ cutList (treeLeftOffset config) (treeSize config) inputU8
    treeRight = map fromIntegral $ cutList (treeLeftOffset config + treeSize config) (treeSize config) inputU8
    charMap =  cutList (charMapOffset config) (charMapSize config) inputU8
    pointerTable = cutList (pointerTableOffset config) (pointerTableSize config) inputU8
    pointers = init $ getPointers pointerTable --last pointer is unused and zeroed
    hTree = buildTree treeLeft treeRight charMap
    compressedBlocks = map (getBitStream input (textBase config)) pointers
    --each block contains 4 screen messages:
  in concatMap (head . splitFourScreens (endOfScreen config) . decode hTree) compressedBlocks

-- from a list of bits, navigate a given huffman tree and emit its decoded
-- symbol when reaching a Leaf
-- stop at endOfScreen val of Leaf
decode:: HuffmanTree a -> [Bool] -> [a]
decode treeRoot xs0 = go treeRoot xs0
  where
    -- reached leaf, emit symbol
    go (Leaf _ c) bs = c : go treeRoot bs
    -- choose path based on bit
    go (Tree _ l r) (b:bs)
      | not b = go l bs
      | otherwise = go r bs
    go _ [] = []


-- count the number of instances each symbol occurs in a list
-- tuples are swapped, as map had fst as Key, and we should have [(weight, char)] tuples
histogram :: Ord a => [a] -> [(Int,a)]
histogram xs = swap . M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]
  where swap = map (\(a,b)->(b,a))

-- builds huffman tree from given left and right tree in Battletoads format
-- and charmap
buildTree :: [Int] -> [Int] -> [a] -> HuffmanTree a
buildTree tL tR charMap = go 0 
  where 
      go off = Tree off (check (tL !! off))  (check (tR !! off)) 
        where 
        check t = if t >= 0x80 then Leaf off (charMap !! (t - 0x80)) else go t 

--takes size of list beginning from offset
cutList :: Int -> Int -> [a] -> [a]
cutList offset size xs = take size $ drop  offset xs 

getPointers :: [Word8] -> [(Int,Int)]--convert ptr table to tuples (offset, startingBit)
getPointers xs = map convert $ chunksOf 2 $ map fromIntegral xs
  where convert [lo, hi] = ((hi `shiftR` 3) * 0x100 + lo, 7 - (hi .&. 7))

calcPointers :: [[Bool]] -> [Word8]
calcPointers bs = 
  let lengths = 0 : map length bs
      lengthsAccum = scanl1 (+) lengths
      offsets = map (`div` 8) lengthsAccum
      startBits = map (\l -> 7 - (l `rem` 8)) lengthsAccum
      tuples = zip offsets startBits
      convert (offset, start) = [fromIntegral(offset .&. 0xFF), fromIntegral(((offset `shiftR` 5) .&. 0xF8) .|. start)]
  in concatMap convert tuples --last chunk does not create pointer, but sits in game's pointer table


--get bitstream from ByteString and tuple (offset, starting bit)
getBitStream :: Bs.ByteString -> Int -> (Int, Int) -> [Bool]
getBitStream input textBase' ptr =  drop (snd ptr) $ Bi.unpack (Bi.fromByteString bytes :: Bi.Bitstream Bi.Right)
  where bytes = Bs.drop (fromIntegral textBase' + fromIntegral(fst ptr)) input

----------------------------------------Config part-------------------------------------------
data ConfigInfo = ConfigInfo {  textBankOffset,
                                treeSize,
                                treeLeftOffset,
                                pointerTableSize :: Int,
                                endOfScreen :: Word8,
                                charMapOffset,
                                charMapSize,
                                pointerTableOffset,
                                textBase :: Int 
                             }

readConfig :: String -> IO ConfigInfo
readConfig f = do
   rv <- runErrorT $ do
      -- open the configuration file
      cp <- join $ liftIO $ readfile emptyCP{ optionxform = id } f
      tbo <- get cp "" "textBankOffset"
      ts <- get cp "" "treeSize"
      tlo' <- get cp "" "treeLeftOffset"
      let tlo = tlo' + tbo
      pts <- get cp "" "pointerTableSize"
      eos <- get cp "" "endOfScreen"
      let
        cmo = tlo + 2* ts
        cms = ts + 1
        pto = cmo + cms
        tb = pto + pts

      -- build the config value
      return $ ConfigInfo tbo ts tlo pts eos cmo cms pto tb 
   either (\x -> error("Cfg error in " ++ snd x))  return rv


----------------------------------------Command line parse part----------------------------------

data Action = Decode | Encode | NoAction deriving (Show, Eq)
data Options = Options
              {optHelp :: Bool
              ,optVersion :: Bool
              ,optAction :: Action
              }
              deriving (Show)
defaultOptions :: Options
defaultOptions = Options
                  {optHelp = False
                  ,optVersion = False
                  ,optAction = NoAction
                  }

usage :: String
usage = usageInfo "Usage: habsentminded [-d|e] [input_file output_file]" options

options :: [OptDescr (Options -> Options)]
options =
  [ Option "d"     ["decode"]  (NoArg (\opts -> opts {optAction = Decode}))  "decode from ROM"
  , Option "e"     ["encode"]  (NoArg (\opts -> opts {optAction = Encode}))  "encode from raw binary text"
        , Option "h?" ["help"]    (NoArg (\ opts -> opts { optHelp = True }))   "show help"
        , Option "v" ["version"]     (NoArg (\ opts -> opts { optVersion = True })) "show version number"
  ]


habsentOpts :: [String] -> IO (Options, [String])
habsentOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))

----------------------------------------------Main------------------------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  (opts, fnames) <- habsentOpts argv
  when (optVersion opts) $ do
    putStrLn "Habsentminded. NES Battletoads series text utility. Version 0.9"
    exitSuccess
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  let action = optAction opts
  when ((length fnames /= 2) || (action == NoAction)) $ do
    putStrLn "Supply action flag and two filenames\n"
    putStrLn usage
    exitFailure 
  let [inputFileName, outputFileName] = fnames
      configFileName = "habsentminded.cfg"
  if action == Decode
    then do  
      input <- Bs.readFile inputFileName
      config <- readConfig configFileName
      let out = decodeText config input
      Bs.writeFile outputFileName (Bs.pack out)
      
    else do --encoding
      input <- Bs.readFile inputFileName
      config <- readConfig configFileName --take endOfScreen symbol
      let inputU8 =  Bs.unpack input
          sortedFrequencies = sort $ histogram inputU8
          charMap = reverse $ map snd sortedFrequencies--extract only chars from max to min freq
          huffmanTree = sortedHuffman sortedFrequencies
          --all left trees and leafs has even index in overall list of nodes, got from
          --treeLevels. Split them in two different trees.
          levels = splitOddIndex $ treeLevels huffmanTree charMap
          treeLeft = fst levels
          treeRight = snd levels
          codeTable = codes huffmanTree 
          blocks = splitFourScreens (endOfScreen config) inputU8
          encodedBlocks = map (encode codeTable) blocks
          pointers = calcPointers encodedBlocks
          encodedStr = concat encodedBlocks
          out = treeLeft ++ treeRight ++ charMap ++ pointers 
      Bs.writeFile outputFileName (Bs.pack out)
      Bi.appendFile outputFileName (Bi.pack encodedStr :: Bi.Bitstream Bi.Right)
      
