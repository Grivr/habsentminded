Habsentminded
=============

Absentminded rewritten in Haskell for a great good.
Habsentminded, as well as my previous Absentminded, is a NES Battletoads series text tool. Supports Battletoads (U).nes and Battletoads & Double Dragon - The Ultimate Team (U).nes

The text is packed with Huffman, so Habsentminded can decode whole text to a binary file (which can be edited by any script translation tool you like) and then encode changed text in order to insert in back to ROM. For that, I use my utility binInsert - see a bat file for details.

```
Usage: habsentminded [-d|e] [input_file output_file]
  -d      --decode   decode from ROM
  -e      --encode   encode from raw binary text
  -h, -?  --help     show help
  -v      --version  show version number