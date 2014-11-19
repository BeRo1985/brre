program BRRETest;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef win32}
 {$apptype console}
{$endif}

uses
  SysUtils,
  Classes,
  BRRE in 'BRRE.pas',
  BRREUnicode in 'BRREUnicode.pas';

var RegExp:TBRRERegExp;
    MatchAll:TBRRERegExpMultipleStrings;
begin
(*try
  RegExp:=TBRRERegExp.Create('/o{0,300}/');
  try
   MatchAll:=RegExp.MatchAll('ooooooooooooooooooooa');
   if length(MatchAll)>0 then begin
    writeln(MatchAll[0,0]);
   end;
  finally
   RegExp.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 readln;
 exit;(**)
 try
  RegExp:=TBRRERegExp.Create('/one(?:self)?(?:selfsufficient)?/');
  try
   MatchAll:=RegExp.ExtractAll('oneself');
   if length(MatchAll)>0 then begin
    writeln(MatchAll[0,0]);
   end;
  finally
   RegExp.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 try
  RegExp:=TBRRERegExp.Create('/test[0-9]/');
  try
   writeln(RegExp.Replace('My test2 for a test is a good test4 for a test','day'));
  finally
   RegExp.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 try
  RegExp:=TBRRERegExp.Create('/<([A-Z][A-Z0-9]*)\b[^>]*>(.*?)</\1>( with a)/i');
  try
   writeln(RegExp.Replace('<a href="http://www.regexp.net">No link</a> with a text','$2 without a'));
  finally
   RegExp.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 try
  RegExp:=TBRRERegExp.Create('/[\-\_]/');
  try
   writeln(RegExp.Replace('This-is_a_test',' '));
  finally
   RegExp.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 try
  RegExp:=TBRRERegExp.Create('/\b([A-Z0-9._%+-]+)@([A-Z0-9.-]+\.[A-Z]{2,4})\b/i');
  try
   writeln(RegExp.Replace('user@domain.com','$1 is at $2 and his email address is $0'));
  finally
   RegExp.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 try
  RegExp:=TBRRERegExp.Create('/test/');
  try
   writeln(RegExp.Replace('My test is a good test','day'));
  finally
   RegExp.Free;
  end;
 except
  on e:Exception do begin
   writeln(e.Message);
  end;
 end;
 readln;
end.
