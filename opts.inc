//n.b. We now define FASTGZ in the .lpi file
//{$DEFINE FASTGZ}
//{$IFDEF DARWIN}{$IFDEF CPUAARCH64} {$DEFINE LIBDEFLATE} {$ENDIF}{$ENDIF}
//use libdeflate if both FASTGZ and LIBDEFLATE are set
//use synzip CloudFlare if FASTGZ but not
//use Pascal zlib if not FASTGZ
//faster GZ decompression, requires 
//  Windows only : if you get an error "Can't find object file" you can copy the 'static' folder from
//    https://github.com/synopse/mORMot
//{$DEFINE PARALLEL}  //for parallel Unix also edit CThreads in LPR file - no benefit if used with DYNRGBA
//{$DEFINE DYNRGBA}  //{$IFDEF DYNRGBA}
{$DEFINE MOSAIC}
{$DEFINE LINE3D}
{$DEFINE DEPTHPICKER} //use a separate shader
{$DEFINE DEPTHPICKER2}
//{$DEFINE NEWPY} //It is suggested to use 2020.07.31 or earlier. This switch is for new versions https://github.com/Alexey-T/Python-for-Lazarus/issues/25
