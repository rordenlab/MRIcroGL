REM COMPILE MRIcroGL12
cd c:\pas\MRIcroGL12
c:\lazarus\lazbuild --cpu=x86_64 -B MRIcroGL.lpi
move /Y "C:\pas\MRIcroGL12\MRIcroGL.exe" "c:\mricrogl12\MRIcroGL.exe"

del /S *.~*
del /S .DS_STORE
del /S *.dcu
del /S *.hpp
del /S *.ddp
del /S *.mps
del /S *.mpt
del /S *.dsm
MRIcroGL.exe

c:\Progra~1\7-Zip\7z a -tzip c:\pas\mricrogl12_source.zip c:\pas\MRIcroGL12

c:\Progra~1\7-Zip\7z a -tzip c:\pas\mricrogl12_windows.zip c:\MRIcroGL12
