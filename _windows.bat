REM COMPILE MRIcroGL paths must be changed as required
cd d:\pas\MRIcroGL
REM optional fastgz
REM copy fastgz.inc opts.inc /y
d:\lazarus\lazbuild --cpu=x86_64 -B MRIcroGL.lpi
move /Y "D:\pas\MRIcroGL\MRIcroGL.exe" "d:\neuro\mricrogl\MRIcroGL.exe"

del /S *.~*
del /S .DS_STORE
del /S *.dcu
del /S *.hpp
del /S *.ddp
del /S *.mps
del /S *.mpt
del /S *.dsm
del MRIcroGL.exe

c:\Progra~1\7-Zip\7z a -tzip d:\pas\mricrogl_source.zip d:\pas\MRIcroGL12

c:\Progra~1\7-Zip\7z a -tzip d:\MRIcroGL_windows.zip d:\neuro\MRIcroGL
