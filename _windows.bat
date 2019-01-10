REM COMPILE MRIcroGL12
cd d:\pas\MRIcroGL12
d:\lazarus\lazbuild --cpu=x86_64 -B MRIcroGL.lpi
move /Y "D:\pas\MRIcroGL12\MRIcroGL.exe" "d:\neuro\mricrogl12\MRIcroGL.exe"

del /S *.~*
del /S .DS_STORE
del /S *.dcu
del /S *.hpp
del /S *.ddp
del /S *.mps
del /S *.mpt
del /S *.dsm
MRIcroGL.exe

c:\Progra~1\7-Zip\7z a -tzip d:\pas\mricrogl12_source.zip d:\pas\MRIcroGL12

c:\Progra~1\7-Zip\7z a -tzip d:\mricrogl12_windows.zip d:\neuro\MRIcroGL12
