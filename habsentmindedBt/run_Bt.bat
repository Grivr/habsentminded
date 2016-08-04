echo off
set romName="Battletoads (U).nes"
set treeLeftOffset=0x3664B
set encodedTextSize=0x184D
del *.bin
echo on
habsentmindedBt -d %romName% text.bin
echo "Decoded"
:loop
	pause
	habsentmindedBt -e text.bin encoded.bin
	insertBin encoded.bin %romName% -o %treeLeftOffset% -s %encodedTextSize%
	echo "Inserted"
goto :loop