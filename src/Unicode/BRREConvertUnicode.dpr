program BRREConvertUnicode;
(**********************************************************
** BRRE Regular Expression Library                        *
***********************************************************
**
** This file is part of the BRRE Regular Expression Library.
** Copyright (C) 2011 by Benjamin Rosseaux
**
** See the file COPYING.BRRE, included in this distribution,
** for details about the copyright.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef win32}
 {$apptype console}
{$endif}
{$ifdef win64}
 {$apptype console}
{$endif}
uses SysUtils,Classes;

const MaxUnicodeChar=$10ffff;
      CountUnicodeChars=$110000;

type TBRREUnicodeDWords=array[0..MaxUnicodeChar] of longint;

var BRREUnicodeCategories:TBRREUnicodeDWords;
    BRREUnicodeScripts:TBRREUnicodeDWords;
    BRREUnicodeLowerCaseDeltas:TBRREUnicodeDWords;
    BRREUnicodeUpperCaseDeltas:TBRREUnicodeDWords;
    BRREUnicodeTitleCaseDeltas:TBRREUnicodeDWords;
    BRRECategories:TStringList;
    BRREScripts:TStringList;
    OutputList:TStringList;

function GetUntilSplitter(const Splitter:ansistring;var s:ansistring):ansistring;
var i:longint;
begin
 i:=pos(Splitter,s);
 if i>0 then begin
  result:=trim(copy(s,1,i-1));
  Delete(s,1,(i+length(Splitter))-1);
  s:=trim(s);
 end else begin
  result:=trim(s);
  s:='';
 end;
end;

procedure ParseBlocks;
type TBRREUnicodeBlock=record
      Name:ansistring;
      FromChar,ToChar:longword;
     end;
var List:TStringList;
    i,j,k,FromChar,ToChar,Count:longint;
    s,p:ansistring;
    Blocks:array of TBRREUnicodeBlock;
begin
 Blocks:=nil;
 try
  Count:=0;
  OutputList.Add('type TBRREUnicodeBlock=record');
  OutputList.Add('      Name:ansistring;');
  OutputList.Add('      FromChar,ToChar:longword;');
  OutputList.Add('     end;');
  List:=TStringList.Create;
  try
   List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'Blocks.txt');
   for i:=0 to List.Count-1 do begin
    s:=trim(List[i]);
    if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
     continue;
    end;
    j:=pos('#',s);
    if j>0 then begin
     s:=trim(copy(s,1,j-1));
    end;
    j:=pos(';',s);
    if j=0 then begin
     continue;
    end;
    p:=trim(copy(s,j+1,length(s)-j));
    s:=trim(copy(s,1,j-1));
    j:=pos('..',s);
    if j=0 then begin
     FromChar:=StrToInt('$'+trim(s));
     ToChar:=FromChar;
    end else begin
     FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
     ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    end;
    if (Count+1)>=length(Blocks) then begin
     j:=1;
     k:=Count+1;
     while j<=k do begin
      inc(j,j);
     end;
     SetLength(Blocks,j);
    end;
    Blocks[Count].Name:=p;
    Blocks[Count].FromChar:=FromChar;
    Blocks[Count].ToChar:=ToChar;
    inc(Count);
   end;
   SetLength(Blocks,Count);
  finally
   List.Free;
  end;
  OutputList.Add('const BRREUnicodeBlockCount='+IntToStr(Count)+';');
  OutputList.Add('      BRREUnicodeBlocks:array[0..'+IntToStr(Count-1)+'] of TBRREUnicodeBlock=(');
  for i:=0 to Count-1 do begin
   if (i+1)<Count then begin
    OutputList.Add('       (Name:'''+Blocks[i].Name+''';FromChar:'+inttostr(Blocks[i].FromChar)+';ToChar:'+inttostr(Blocks[i].ToChar)+'),');
   end else begin
    OutputList.Add('       (Name:'''+Blocks[i].Name+''';FromChar:'+inttostr(Blocks[i].FromChar)+';ToChar:'+inttostr(Blocks[i].ToChar)+'));');
   end;
  end;
  if Count=0 then begin
   OutputList.Add(');');
  end;
  OutputList.Add('');
 finally
  SetLength(Blocks,0);
 end;
end;

procedure ParseDerivedGeneralCategory;
var List:TStringList;
    i,j,ci,FromChar,ToChar,CurrentChar:longint;
    s,p:ansistring;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'DerivedGeneralCategory.txt');
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   p:=trim(copy(s,j+1,length(s)-j));
   ci:=BRRECategories.IndexOf(p);
   if ci<0 then begin
    ci:=BRRECategories.Add(p);
   end;
   s:=trim(copy(s,1,j-1));
   j:=pos('..',s);
   if j=0 then begin
    CurrentChar:=StrToInt('$'+trim(s));
    BRREUnicodeCategories[CurrentChar]:=ci;
   end else begin
    FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
    ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    for CurrentChar:=FromChar to ToChar do begin
     BRREUnicodeCategories[CurrentChar]:=ci;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure ParseScripts;
var List:TStringList;
    i,j,si,FromChar,ToChar,CurrentChar:longint;
    s,p:ansistring;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'Scripts.txt');
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   p:=trim(copy(s,j+1,length(s)-j));
   si:=BRREScripts.IndexOf(p);
   if si<0 then begin
    si:=BRREScripts.Add(p);
   end;
   s:=trim(copy(s,1,j-1));
   j:=pos('..',s);
   if j=0 then begin
    CurrentChar:=StrToInt('$'+trim(s));
    BRREUnicodeScripts[CurrentChar]:=si;
   end else begin
    FromChar:=StrToInt('$'+trim(copy(s,1,j-1)));
    ToChar:=StrToInt('$'+trim(copy(s,j+2,length(s)-(j+1))));
    for CurrentChar:=FromChar to ToChar do begin
     BRREUnicodeScripts[CurrentChar]:=si;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure ParseUnicodeData;
var List:TStringList;
    i,j,ci,OtherChar,CurrentChar:longint;
    s,cs:ansistring;
begin
 List:=TStringList.Create;
 try
  List.LoadFromFile(IncludeTrailingPathDelimiter('UnicodeData')+'UnicodeData.txt');
  for i:=ord('a') to ord('z') do begin
   BRREUnicodeUpperCaseDeltas[i]:=longint(ord('A')-ord('a'));
  end;
  for i:=ord('A') to ord('Z') do begin
   BRREUnicodeLowerCaseDeltas[i]:=ord('a')-ord('A');
  end;
  for i:=$ff21 to $ff3a do begin
   BRREUnicodeLowerCaseDeltas[i]:=$ff41-$ff21;
  end;
  for i:=$ff41 to $ff5a do begin
   BRREUnicodeUpperCaseDeltas[i]:=longint($ff21-$ff41);
  end;
  for i:=0 to List.Count-1 do begin
   s:=trim(List[i]);
   if (length(s)=0) or ((length(s)>0) and (s[1]='#')) then begin
    continue;
   end;
   j:=pos('#',s);
   if j>0 then begin
    s:=trim(copy(s,1,j-1));
   end;
   j:=pos(';',s);
   if j=0 then begin
    continue;
   end;
   CurrentChar:=StrToInt('$'+GetUntilSplitter(';',s)); // Code
   GetUntilSplitter(';',s); // Name
   begin
    cs:=GetUntilSplitter(';',s); // Class
    ci:=BRRECategories.IndexOf(cs);
    if ci<0 then begin
     ci:=BRRECategories.Add(cs);
    end;
    if BRREUnicodeCategories[CurrentChar]<>ci then begin
     writeln(ErrOutput,CurrentChar,' has multiple categories?');
     BRREUnicodeCategories[CurrentChar]:=ci;
    end;
   end;
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   GetUntilSplitter(';',s); // ???
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // UpperChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     BRREUnicodeUpperCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // LowerChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     BRREUnicodeLowerCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
   begin
    OtherChar:=StrToIntDef('$'+GetUntilSplitter(';',s),-1); // TitleChar Code
    if (OtherChar>=0) and (OtherChar<>CurrentChar) then begin
     BRREUnicodeTitleCaseDeltas[CurrentChar]:=OtherChar-CurrentChar;
    end;
   end;
  end;
 finally
  List.Free;
 end;
end;

procedure PackTable(const Table:array of longint;Level:integer;const Name:ansistring);
type TBlock=array of longint;
     TBlocks=array of TBlock;
     TIndices=array of longint;
var BestBlockSize,BlockSize,CountBlocks,CountIndices,Index,BlockPosition,Bytes,BestBytes,Bits,BestBits,EntryBytes,IndicesEntryBytes,BestIndicesEntryBytes,i,j,k:longint;
    Block:TBlock;
    Blocks:TBlocks;
    Indices:TIndices;
    BestBlocks:TBlocks;
    BestIndices:TIndices;
    OK:boolean;
    s:ansistring;
begin
 if Level<2 then begin
  Block:=nil;
  Blocks:=nil;
  Indices:=nil;
  BestBlocks:=nil;
  BestIndices:=nil;
  try
   BestBlockSize:=length(Table)*2;
   BestBits:=24;
   BlockSize:=1;
   Bits:=0;
   BestBytes:=-1;
   i:=0;
   OK:=true;
   for Index:=0 to length(Table)-1 do begin
    j:=Table[Index];
    if j<0 then begin
     OK:=false;
    end;
    j:=abs(j);
    if i<j then begin
     i:=j;
    end;
   end;
   if OK then begin
    if i<256 then begin
     EntryBytes:=1;
     s:='byte';
    end else if i<65536 then begin
     EntryBytes:=2;
     s:='word';
    end else begin
     EntryBytes:=4;
     s:='longword';
    end;
   end else begin
    if i<128 then begin
     EntryBytes:=1;
     s:='shortint';
    end else if i<32768 then begin
     EntryBytes:=2;
     s:='smallint';
    end else begin
     EntryBytes:=4;
     s:='longint';
    end;
   end;
   BestIndicesEntryBytes:=4;
   while BlockSize<length(Table) do begin
    SetLength(Block,BlockSize);
    SetLength(Blocks,(length(Table) div BlockSize)+1);
    FillChar(Block[0],BlockSize,#$ff);
    BlockPosition:=0;
    CountBlocks:=0;
    CountIndices:=0;
    for Index:=0 to length(Table)-1 do begin
     Block[BlockPosition]:=Table[Index];
     inc(BlockPosition);
     if BlockPosition=BlockSize then begin
      k:=-1;
      for i:=0 to CountBlocks-1 do begin
       OK:=true;
       for j:=0 to BlockSize-1 do begin
        if Blocks[i,j]<>Block[j] then begin
         OK:=false;
         break;
        end;
       end;
       if OK then begin
        k:=i;
        break;
       end;
      end;
      if k<0 then begin
       k:=CountBlocks;
       Blocks[CountBlocks]:=copy(Block);
       inc(CountBlocks);
      end;
      if (CountIndices+1)>=length(Indices) then begin
       i:=1;
       j:=CountIndices+1;
       while i<=j do begin
        inc(i,i);
       end;
       SetLength(Indices,i);
      end;
      Indices[CountIndices]:=k;
      inc(CountIndices);
      BlockPosition:=0;
     end;
    end;
    if CountBlocks<256 then begin
     IndicesEntryBytes:=1;
    end else if CountBlocks<65536 then begin
     IndicesEntryBytes:=2;
    end else begin
     IndicesEntryBytes:=4;
    end;
    Bytes:=((CountBlocks*BlockSize)*EntryBytes)+(CountIndices*IndicesEntryBytes);
    if (BestBytes<0) or (Bytes<=BestBytes) then begin
     BestBytes:=Bytes;
     BestBlockSize:=BlockSize;
     BestBits:=Bits;
     BestIndicesEntryBytes:=EntryBytes;
     BestBlocks:=copy(Blocks,0,CountBlocks);
     BestIndices:=copy(Indices,0,CountIndices);
    end;
    SetLength(Blocks,0);
    SetLength(Indices,0);
    inc(BlockSize,BlockSize);
    inc(Bits);
   end;
   OutputList.Add('// '+Name+': '+IntToStr(BestBytes)+' bytes, '+IntToStr(length(BestBlocks))+' blocks with '+IntToStr(BestBlockSize)+' items per '+IntToStr(EntryBytes)+' bytes and '+IntToStr(length(BestIndices))+' indices per '+IntToStr(BestIndicesEntryBytes)+' bytes');
   OutputList.Add('const '+Name+'BlockBits='+IntToStr(BestBits)+';');
   OutputList.Add('      '+Name+'BlockMask='+IntToStr((1 shl BestBits)-1)+';');
   OutputList.Add('      '+Name+'BlockSize='+IntToStr(BestBlockSize)+';');
   OutputList.Add('      '+Name+'BlockCount='+IntToStr(length(BestBlocks))+';');
   OutputList.Add('      '+Name+'BlockData:array[0..'+IntToStr(length(BestBlocks)-1)+',0..'+IntToStr(BestBlockSize-1)+'] of '+s+'=(');
   s:='';
   for i:=0 to length(BestBlocks)-1 do begin
    s:=s+'(';
    for j:=0 to BestBlockSize-1 do begin
     s:=s+IntToStr(BestBlocks[i,j]);
     if (j+1)<BestBlockSize then begin
      s:=s+',';
     end;
     if length(s)>80 then begin
      OutputList.Add(s);
      s:='';
     end;
    end;
    s:=s+')';
    if (i+1)<length(BestBlocks) then begin
     s:=s+',';
    end;
    OutputList.Add(s);
    s:='';
   end;
   if length(s)>0 then begin
    OutputList.Add(s);
    s:='';
   end;
   OutputList.Add(');');
   if Level=1 then begin
    case BestIndicesEntryBytes of
     1:begin
      s:='byte';
     end;
     2:begin
      s:='word';
     end;
     else begin
      s:='longword';
     end;
    end;
    OutputList.Add('      '+Name+'IndexCount='+IntToStr(length(BestBlocks))+';');
    OutputList.Add('      '+Name+'IndexData:array[0..'+IntToStr(length(BestIndices)-1)+'] of '+s+'=(');
    s:='';
    for i:=0 to length(BestIndices)-1 do begin
     s:=s+IntToStr(BestIndices[i]);
     if (i+1)<length(BestIndices) then begin
      s:=s+',';
     end;
     if length(s)>80 then begin
      OutputList.Add(s);
      s:='';
     end;
    end;
    if length(s)>0 then begin
     OutputList.Add(s);
     s:='';
    end;
    OutputList.Add(');');
    OutputList.Add('');
   end else begin
    OutputList.Add('');
    PackTable(BestIndices,Level+1,Name+'Index');
   end;
  finally
   SetLength(Block,0);
   SetLength(Blocks,0);
   SetLength(Indices,0);
   SetLength(BestBlocks,0);
   SetLength(BestIndices,0);
  end;
 end;
end;

var i:longint;
begin
 FillChar(BRREUnicodeCategories,sizeof(TBRREUnicodeDWords),#0);
 FillChar(BRREUnicodeScripts,sizeof(TBRREUnicodeDWords),#$0);
 FillChar(BRREUnicodeUpperCaseDeltas,sizeof(TBRREUnicodeDWords),#$0);
 FillChar(BRREUnicodeLowerCaseDeltas,sizeof(TBRREUnicodeDWords),#$0);
 FillChar(BRREUnicodeTitleCaseDeltas,sizeof(TBRREUnicodeDWords),#$0);
 OutputList:=TStringList.Create;
 try
  BRRECategories:=TStringList.Create;
  BRRECategories.Add('Cn');
  try
   BRREScripts:=TStringList.Create;
   BRREScripts.Add('Unknown');
   BRREScripts.Add('Common');
   try
    ParseDerivedGeneralCategory;
    ParseScripts;
    ParseUnicodeData;
    OutputList.Add('unit BRREUnicode;');
    OutputList.Add('{$ifdef fpc}');
    OutputList.Add(' {$mode delphi}');
    OutputList.Add('{$endif}');
    OutputList.Add('interface');
    OutputList.Add('');
    ParseBlocks;
    begin
     OutputList.Add('const BRREUnicodeCategoryIDs:array[0..'+IntToStr(BRRECategories.Count-1)+'] of ansistring=(');
     for i:=0 to BRRECategories.Count-1 do begin
      if (i+1)<BRRECategories.Count then begin
       OutputList.Add(''''+BRRECategories[i]+''',');
      end else begin
       OutputList.Add(''''+BRRECategories[i]+'''');
      end;
     end;
     OutputList.Add(');');
     for i:=0 to BRRECategories.Count-1 do begin
      OutputList.Add('      BRREUnicodeCategory'+BRRECategories[i]+'='+IntToStr(i)+';');
     end;
     OutputList.Add('      BRREUnicodeCategoryCount='+IntToStr(BRRECategories.Count)+';');
     OutputList.Add('      BRRE_CT_UNASSIGNED=BRREUnicodeCategoryCn;');
     OutputList.Add('      BRRE_CT_UPPERCASE_LETTER=BRREUnicodeCategoryLu;');
     OutputList.Add('      BRRE_CT_LOWERCASE_LETTER=BRREUnicodeCategoryLl;');
     OutputList.Add('      BRRE_CT_TITLECASE_LETTER=BRREUnicodeCategoryLt;');
     OutputList.Add('      BRRE_CT_MODIFIER_LETTER=BRREUnicodeCategoryLm;');
     OutputList.Add('      BRRE_CT_OTHER_LETTER=BRREUnicodeCategoryLo;');
     OutputList.Add('      BRRE_CT_NON_SPACING_MARK=BRREUnicodeCategoryMn;');
     OutputList.Add('      BRRE_CT_ENCLOSING_MARK=BRREUnicodeCategoryMe;');
     OutputList.Add('      BRRE_CT_COMBINING_SPACING_MARK=BRREUnicodeCategoryMc;');
     OutputList.Add('      BRRE_CT_DECIMAL_DIGIT_NUMBER=BRREUnicodeCategoryNd;');
     OutputList.Add('      BRRE_CT_LETTER_NUMBER=BRREUnicodeCategoryNl;');
     OutputList.Add('      BRRE_CT_OTHER_NUMBER=BRREUnicodeCategoryNo;');
     OutputList.Add('      BRRE_CT_SPACE_SEPARATOR=BRREUnicodeCategoryZs;');
     OutputList.Add('      BRRE_CT_LINE_SEPARATOR=BRREUnicodeCategoryZl;');
     OutputList.Add('      BRRE_CT_PARAGRAPH_SEPARATOR=BRREUnicodeCategoryZp;');
     OutputList.Add('      BRRE_CT_CONTROL=BRREUnicodeCategoryCc;');
     OutputList.Add('      BRRE_CT_FORMAT=BRREUnicodeCategoryCf;');
     OutputList.Add('      BRRE_CT_PRIVATE_USE=BRREUnicodeCategoryCo;');
     OutputList.Add('      BRRE_CT_SURROGATE=BRREUnicodeCategoryCs;');
     OutputList.Add('      BRRE_CT_DASH_PUNCTUATION=BRREUnicodeCategoryPd;');
     OutputList.Add('      BRRE_CT_START_PUNCTUATION=BRREUnicodeCategoryPs;');
     OutputList.Add('      BRRE_CT_END_PUNCTUATION=BRREUnicodeCategoryPe;');
     OutputList.Add('      BRRE_CT_INITIAL_PUNCTUATION=BRREUnicodeCategoryPi;');
     OutputList.Add('      BRRE_CT_FINAL_PUNCTUATION=BRREUnicodeCategoryPf;');
     OutputList.Add('      BRRE_CT_CONNECTOR_PUNCTUATION=BRREUnicodeCategoryPc;');
     OutputList.Add('      BRRE_CT_OTHER_PUNCTUATION=BRREUnicodeCategoryPo;');
     OutputList.Add('      BRRE_CT_MATH_SYMBOL=BRREUnicodeCategorySm;');
     OutputList.Add('      BRRE_CT_CURRENCY_SYMBOL=BRREUnicodeCategorySc;');
     OutputList.Add('      BRRE_CT_MODIFIER_SYMBOL=BRREUnicodeCategorySk;');
     OutputList.Add('      BRRE_CT_OTHER_SYMBOL=BRREUnicodeCategorySo;');
     OutputList.Add('');
    end;
    begin
     OutputList.Add('const BRREUnicodeScriptIDs:array[0..'+IntToStr(BRREScripts.Count-1)+'] of ansistring=(');
     for i:=0 to BRREScripts.Count-1 do begin
      if (i+1)<BRREScripts.Count then begin
       OutputList.Add(''''+BRREScripts[i]+''',');
      end else begin
       OutputList.Add(''''+BRREScripts[i]+'''');
      end;
     end;
     OutputList.Add(');');
     for i:=0 to BRREScripts.Count-1 do begin
      OutputList.Add('     BRREUnicodeScript'+BRREScripts[i]+'='+IntToStr(i)+';');
     end;
     OutputList.Add('     BRREUnicodeScriptCount='+IntToStr(BRREScripts.Count)+';');
     OutputList.Add('');
    end;
    PackTable(BRREUnicodeCategories,0,'BRREUnicodeCategoryArray');
    writeln;
    PackTable(BRREUnicodeScripts,0,'BRREUnicodeScriptArray');
    writeln;
    PackTable(BRREUnicodeUpperCaseDeltas,0,'BRREUnicodeUpperCaseDeltaArray');
    PackTable(BRREUnicodeLowerCaseDeltas,0,'BRREUnicodeLowerCaseDeltaArray');
    PackTable(BRREUnicodeTitleCaseDeltas,0,'BRREUnicodeTitleCaseDeltaArray');
    OutputList.Add('implementation');
    OutputList.Add('end.');
    OutputList.SaveToFile(IncludeTrailingPathDelimiter('..')+'BRREUnicode.pas');
   finally
    BRREScripts.Free;
   end;
  finally
   BRRECategories.Free;
  end;
 finally
  OutputList.Free;
 end;
end.
