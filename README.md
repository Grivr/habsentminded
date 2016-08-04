Habsentminded
=============

Absentminded rewritten in Haskell for a great good.
Habsentminded, as well as my previous Absentminded, is a NES Battletoads series text tool. Supports Battletoads (U).nes and Battletoads & Double Dragon - The Ultimate Team (U).nes

The text is packed with Huffman, so Habsentminded can decode whole text to a binary file (which can be edited by any script translation tool you like) and then encode changed text in order to insert in back to ROM. For that, I use my utility binInsert - see a bat files for details. The tool is shipped with 2 batch files for each of games.

As Battletoads use last text block for level names storage only and Battletoads & Double Dragon - The Ultimate Team just keep this pointer and block void, decoding logic differs slightly between games.
For Battletoads (U).nes please use habsentmindedBt and for Battletoads & Double Dragon - The Ultimate Team (U).nes use habsentmindedBtDD. Both are placed in appropriate folders as well as they are separated in release archive.