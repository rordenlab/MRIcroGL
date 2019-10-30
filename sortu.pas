unit sortu;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
TSortType = single; //can be integer, single, double, etc
TSort = record
   index:  integer;
   value: TSortType;
end;
TSortArray = array of TSort;

procedure SortArray(var s: TSortArray; method: integer = 0); //method: quick(0), heap(1), shell(2)

implementation

//http://delphi.wikia.com/wiki/Heapsort
procedure HeapSort(var s: TSortArray);
procedure Sink(indx, Arraylength: integer);
var
   item, leftChild, sinkindx, rightChild, parent: integer;
   done: boolean;
begin
   sinkindx := indx;
   item := s[indx].index; // s[s[indx].index].value
   done := False;
   while not done do begin // search sink-path and move up all items
      leftChild := ((sinkindx) * 2) + 1;
      rightChild := ((sinkindx + 1) * 2);
      if rightChild <= Arraylength then begin
         if s[s[leftChild].index].value < s[s[rightChild].index].value then begin
            s[sinkindx].index := s[rightChild].index;  // s[s[leftChild].index].value
            sinkindx := rightChild;
         end
         else begin
           s[sinkindx].index  := s[leftChild].index;
            sinkindx := leftChild;
         end;
      end
      else begin
         done := True;

         if leftChild <= Arraylength then begin
            s[sinkindx].index := s[leftChild].index;
            //Data[sinkindx] := Data[leftChild];
            sinkindx := leftChild;
         end;
      end;
   end;
   // move up current Item
   s[sinkindx].index := item;
   done := False;
   while not done do begin
      parent := Trunc((sinkindx - 1) / 2);
      if (s[s[parent].index].value < s[s[sinkindx].index].value) and (parent >= indx) then begin
         item := s[parent].index;
         s[parent].index := s[sinkindx].index;
         s[sinkindx].index := item;
         sinkindx := parent;
      end
      else
         done := True;
   end;
end;

var
x, b: integer;
begin
  // first make it a Heap
  for x := Trunc((High(s) - 1) / 2) downto Low(s) do
     sink(x, High(s));

  // do the ButtomUpHeap sort
  for x := High(s) downto Low(s) + 1 do begin
     b := s[x].index;
     s[x].index := s[Low(s)].index;
     s[Low(s)].index := b;
     sink(Low(s), x - 1);
  end;
end;

//http://delphidabbler.com/tips/12
procedure ShellSort(var s: TSortArray);
var
  iI, iJ, iK, iSize, wTemp: integer;
begin
  iSize := High(s);
  iK := iSize shr 1;
  while iK > 0 do begin
    for iI := 0 to iSize - iK do begin
      iJ := iI;
      while (iJ >= 0) and (s[s[iJ].index].value > s[s[iJ+iK].index].value) do begin
        wTemp := s[iJ].index;
        s[iJ].index := s[iJ + iK].index;
        s[iJ + iK].index := wTemp;
        if iJ > iK then
          Dec(iJ, iK)
        else
          iJ := 0
      end;
    end;
    iK := iK shr 1;
  end;
end;

//http://stackoverflow.com/questions/24335585/quicksort-drama
procedure QuickSort(left, right: integer; var s: TSortArray);
// left:      Index des 1. Elements, right: Index des letzten Elements
var
  l, r, lswap: integer;
  pivot: TSortType;
begin
  if (right > left) then begin
    l := left;
    r := right;
    pivot := s[s[(right + left) div 2].index].value;
    while (l < r) do begin
      while s[s[l].index].value < pivot do
        l := l + 1;
      while s[s[r].index].value > pivot do
        r := r - 1;
      if (l <= r) then begin
        lswap := s[r].index;
        s[r].index := s[l].index;
        s[l].index := lswap;
        l := l + 1;
        r := r - 1;
      end;
    end;
    if (left < r) then
      QuickSort(left, r, s);
    if (right > l) then
      QuickSort(l, right, s);
  end;
end;
(*procedure QuickSort(left, right: integer; var s: TSortArray);
// left:      Index des 1. Elements, right: Index des letzten Elements
var
  l, r: integer;
  pivot, lswap: TSortType;
begin
  if (right > left) then begin
    l := left;
    r := right;
    pivot := s[s[(right + left) div 2].index].value;
    while (l < r) do begin
      while s[s[l].index].value < pivot do
        l := l + 1;
      while s[s[r].index].value > pivot do
        r := r - 1;
      if (l <= r) then begin
        lswap := s[s[r].index].value;
        s[s[r].index].value := s[s[l].index].value;
        s[s[l].index].value := lswap;
        l := l + 1;
        r := r - 1;
      end;
    end;
    if (left < r) then
      QuickSort(left, r, s);
    if (right > l) then
      QuickSort(l, right, s);
  end;
end; *)

procedure QSort(var s: TSortArray);
begin
  quicksort(low(s), high(s), s);
end;

procedure SortArray(var s: TSortArray; method: integer = 0); //method: quick(0), heap(1), shell(2)
var
 i : integer;
begin
     if length(s) < 1 then exit;
     for i := 0 to (length(s)-1) do  //set indices
         s[i].index := i;
     if method = 1 then
        HeapSort(s)
     else if method = 2 then
         ShellSort (s)
     else
          Qsort(s);
end;

end.

