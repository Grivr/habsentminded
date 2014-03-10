echo off
set romName="Battletoads (U).nes"
set treeLeftOffset=0x3664B
set encodedTextSize=0x17D4
del *.bin
echo on
habsentminded -d %romName% text.bin
echo "Decoded"
:loop
	pause
	habsentminded -e text.bin encoded.bin
	insertBin encoded.bin %romName% -o %treeLeftOffset% -s %encodedTextSize%
	echo "Inserted"
goto :loop