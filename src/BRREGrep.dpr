program BRREGrep;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef win32}
 {$apptype console}
{$endif}

{$ifndef fpc}
{ FastMM4 in 'FastMM4.pas',
  FastMove in 'FastMove.pas',
  FastcodeCPUID in 'FastcodeCPUID.pas',
  FastMM4Messages in 'FastMM4Messages.pas',}
{$endif}

uses
  SysUtils,
  Classes,
  BRRE in 'BRRE.pas',
  BRREUnicode in 'BRREUnicode.pas';

var RegExp:TBRRERegExp;
    //MatchAll:TBRRERegExpMultipleStrings;
    s:ansistring;
    Stream:TStream;
    i,StringLen,StringLenEx,BufferBytes:longint;
    c,lc:ansichar;
    Buffer:array[0..1048575] of ansichar;

procedure FlushString;
var ss:ansistring;
begin
 if StringLen>0 then begin
  ss:=copy(s,1,StringLen);
// MatchAll:=RegExp.MatchAll(ss);
//if length(MatchAll)>0 then begin
  if RegExp.Test(ss) then begin
   writeln(ss);
  end;
  ss:='';
  StringLen:=0;
 end;
end;

begin
 if ParamCount>1 then begin
  Stream:=TFileStream.Create(ParamStr(2),fmOpenRead);
 end;
 try
  RegExp:=TBRRERegExp.Create(AnsiString(ParamStr(1)));
  try
   if ParamCount>1 then begin
    SetLength(s,128);
    lc:=#0;
    StringLen:=0;
    while Stream.Position<Stream.Size do begin
     BufferBytes:=Stream.Read(Buffer,sizeof(Buffer));
     if BufferBytes=0 then begin
      break;
     end;
     for i:=0 to BufferBytes-1 do begin
      c:=Buffer[i];
      if c in [#13,#10] then begin
       if not ((lc in [#13,#10]) and (c<>lc)) then begin
        FlushString;
       end;
      end else begin
       inc(StringLen);
       StringLenEx:=length(s);
       if StringLen>StringLenEx then begin
        repeat
         inc(StringLenEx,StringLenEx);
        until StringLen<=StringLenEx;
        if StringLenEx<>length(s) then begin
         setlength(s,StringLenEx);
        end;
       end;
       s[StringLen]:=c;
      end;
     end;
     FlushString;
    end;
   end else begin
    while not eof do begin
     ReadLn(s);
{    MatchAll:=RegExp.MatchAll(s);
     if length(MatchAll)>0 then begin}
     if RegExp.Test(s) then begin
      writeln(s);
     end;
    end;
   end;
  finally
   RegExp.Free;
   s:='';
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 if ParamCount>1 then begin
  FreeAndNil(Stream);
 end;
end.
