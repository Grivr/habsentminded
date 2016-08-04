echo off
set romName="Battletoads & Double Dragon - The Ultimate Team (U).nes"
set treeLeftOffset=0x1B5BA
set encodedTextSize=0xA76
del *.bin
echo on
habsentmindedBtDD -d %romName% text.bin
echo "Decoded"
:loop
	pause
	habsentmindedBtDD -e text.bin encoded.bin
	insertBin encoded.bin %romName% -o %treeLeftOffset% -s %encodedTextSize%
	echo "Inserted"
goto :loop