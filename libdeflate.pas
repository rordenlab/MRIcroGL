// Copyright (C) 2021 Arnaud Bouche
// low-level access to the zlib/libdeflate API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
// https://github.com/synopse/mORMot2/blob/master/LICENCE.md
// https://github.com/synopse/mORMot2
unit libdeflate;
//{$mode ObjFPC}{$H+}
//{$mode Delphi}{$H+}
{$mode objfpc}{$H+} 
interface


uses
  sysutils,
  classes;

function UncompressMemX(src, dst: pointer; srcLen, dstLen: PtrInt; out cmpBytes: integer;  ZlibFormat: boolean = false): PtrInt;

implementation //535

type
  /// opaque pointer maintaining a libdeflate decompressor instance
  PLibDeflateDecompressor = type pointer;
	  
type
	    TLibDeflateResult = (
	      LIBDEFLATE_SUCCESS,
	      LIBDEFLATE_BAD_DATA,
	      LIBDEFLATE_SHORT_OUTPUT,
	      LIBDEFLATE_INSUFFICIENT_SPACE);



{$ifdef CPUAARCH64} //921
  // current supplied .o don't link yet
   {$L .\z-aarch64-darwin\libdeflate_u.o}  // utils.o
   {$L .\z-aarch64-darwin\libdeflate_cf.o} // cpu_features.o
   {$L .\z-aarch64-darwin\libdeflate_a.o}  // adler32.o
   {$L .\z-aarch64-darwin\libdeflate_c.o}  // crc32.o
   //{$L .\z-aarch64-darwin\libdeflate_dc.o} // deflate_compress.o
   {$L .\z-aarch64-darwin\libdeflate_dd.o} // deflate_decompress.o
   //{$L .\z-aarch64-darwin\libdeflate_zc.o} // zlib_compress.o
   {$L .\z-aarch64-darwin\libdeflate_zd.o} // zlib_decompress.o
{$endif CPUAARCH64}

function libdeflate_alloc_decompressor: PLibDeflateDecompressor; external name '_libdeflate_alloc_decompressor';
function libdeflate_deflate_decompress_ex(
  decompressor: PLibDeflateDecompressor;
  in_buf: pointer; in_nbytes: PtrInt;
  out_buf: pointer; out_nbytes_avail: PtrInt;
  actual_in_nbytes_ret, actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; external name '_libdeflate_deflate_decompress_ex';
function libdeflate_zlib_decompress_ex(
    decompressor: PLibDeflateDecompressor;
    in_buf: pointer; in_nbytes: PtrInt;
    out_buf: pointer; out_nbytes_avail: PtrInt;
    actual_in_nbytes_ret, actual_out_nbytes_ret: PPtrInt): TLibDeflateResult; external name '_libdeflate_zlib_decompress_ex';
procedure libdeflate_free_decompressor(
    decompressor: PLibDeflateDecompressor); external name '_libdeflate_free_decompressor';
		
function UncompressMemX(src, dst: pointer; srcLen, dstLen: PtrInt; out cmpBytes: integer;  ZlibFormat: boolean = false): PtrInt;

var
  dec: PLibDeflateDecompressor;
  res: TLibDeflateResult;
  srcEnd: PtrInt;
begin
  dec := libdeflate_alloc_decompressor;
  if dec = nil then begin
    result := 0;//nil;
	exit;
  end;
  if ZlibFormat then
    res := libdeflate_zlib_decompress_ex(dec, src, srcLen, dst, dstLen, @srcEnd, @result)
  else
    res := libdeflate_deflate_decompress_ex(dec, src, srcLen, dst, dstLen, @srcEnd, @result);
  cmpBytes := srcEnd;//z.Stream.next_in - src;
  libdeflate_free_decompressor(dec);
  if res <> LIBDEFLATE_SUCCESS  then
    result := 0;//nil;
end;


end.

