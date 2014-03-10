Habsentminded
=============

Absentminded rewritten in Haskell for a great good.
Habsentminded, as well as my previous Absentminded, is a NES Battletoads series text tool. Supports Battletoads (U).nes and Battletoads & Double Dragon - The Ultimate Team (U).nes

The text is packed with Huffman, so Habsentminded can decode whole text to a binary file (which can be edited by any script translation tool you like) and then encode changed text in order to insert in back to ROM. For that, I use my utility binInsert - see a bat files for details. The tool is shipped with 2 batch files for each of games.

Habsentminded uses one cfg file for appropriate game. You'll find 2 configs for Battletoads (U).nes and Battletoads & Double Dragon - The Ultimate Team (U).nes. The current config file, which tool uses must have name "habsentminded.cfg". In order to use another config, just rename it.

```
Usage: habsentminded [-d|e] [input_file output_file]
  -d      --decode   decode from ROM
  -e      --encode   encode from raw binary text
  -h, -?  --help     show help
  -v      --version  show version number