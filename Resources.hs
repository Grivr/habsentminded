module Resources where
--import Data.Int (Int64)
import Data.Word (Word8)
--Constants-------------------------------------------------------------
nesHdrSize :: Int
nesHdrSize = 0x10

textBankOffset :: Int 
textBankOffset = 0x10000 + nesHdrSize

--69 symbols can be encoded
treeSize :: Int
treeSize = 0x45
--text data starts here
treeLeftOffs :: Int 
treeLeftOffs = 0xb5aa + textBankOffset

 --charmap after trees
charMapOffs :: Int
charMapOffs = treeLeftOffs + 2*treeSize
charMapSize :: Int
charMapSize = treeSize+1

--14 pointers and text blocks, 16bit words
pointerTableSize :: Int 
pointerTableSize = 14*2
pointerTableOffs :: Int
pointerTableOffs = charMapOffs+charMapSize

--text after pointer table
textBase :: Int
textBase = pointerTableOffs + pointerTableSize

--charcodes:
endOfScreen :: Word8 
endOfScreen = 0xfd
--endOfMsg :: Word8 
--endOfMsg = 0xff
