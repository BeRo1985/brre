program BRREBenchmark;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef win32}
 {$apptype console}
{$endif}

uses
{$ifndef fpc}
  FastMM4 in 'FastMM4.pas',
  FastMove in 'FastMove.pas',
  FastcodeCPUID in 'FastcodeCPUID.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
{$endif}
  Windows,
  SysUtils,
  Classes,
  BRRE in 'BRRE.pas',
  BRREUnicode in 'BRREUnicode.pas',
  Regex in 'regex.pas',
  RegExpr in 'RegExpr.pas',
  pcre,
  PerlRegEx,
  DIRegEx;

// benchmark from http://lh3lh3.users.sourceforge.net/reb.shtml
{const BenchmarkPatterns2:array[0..4] of ansistring=('installation',
                                                    '(?:[a-zA-Z][a-zA-Z0-9]*)://(?:[^ /]+)(?:/[^ ]*)?',
                                                    '(?:[^ @]+)@(?:[^ @]+)',
                                                    '(?:[0-9][0-9]?)/(?:[0-9][0-9]?)/(?:[0-9][0-9](?:[0-9][0-9])?)',
                                                    '(?:[a-zA-Z][a-zA-Z0-9]*)://(?:[^ /]+)(?:/[^ ]*)?|(?:[^ @]+)@(?:[^ @]+)');
 {}
(*const BenchmarkPatterns:array[0..4] of ansistring=('Twain',
                                                   'Huck[a-zA-Z]+',
                                                   '[a-zA-Z]+ing',
                                                   'Tom|Sawyer|Huckleberry|Finn',
                                                   'Tom.{0,30}river|river.{0,30}Tom');
*)
const BenchmarkPatterns:array[0..4] of ansistring=('installation',
                                                   '([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?',
                                                   '([^ @]+)@([^ @]+)',
                                                   '([0-9][0-9]?)/([0-9][0-9]?)/([0-9][0-9]([0-9][0-9])?)',
                                                   '([a-zA-Z][a-zA-Z0-9]*)://([^ /]+)(/[^ ]*)?|([^ @]+)@([^ @]+)');

var i,j,k{,h{}:integer;
    s:ansistring;
    sl:TStringList;
    BRRERegExp:TBRRERegExp;
    RegExp:TRegexEngine;
    RegExprInstance:TRegExpr;
    PerlRegExInstance:TPerlRegEx;
    DIPerlRegEx:TDIPerlRegEx;
    DIDFARegEx:TDIDFARegEx;
    t1,t2:longword;
begin
 sl:=TStringList.Create;
 try
  sl.LoadFromFile('benchmark.txt');
  s:=sl.Text;
  writeln('BRRE:');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    BRRERegExp:=TBRRERegExp.Create('/'+BenchmarkPatterns[i]+'/l');
    try
     t1:=GetTickCount;
(*) for j:=0 to sl.Count-1 do begin
      BRRERegExp.Benchmark(sl[j]);
     end;{(**)
     BRRERegExp.Benchmark(s);{}
     t2:=GetTickCount;
     writeln('/'+BenchmarkPatterns[i]+'/ : ':65,t2-t1:7,' ms');
    finally
     BRRERegExp.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
  writeln('');
  writeln('DIRegEx (DIPerlRegEx):');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    DIPerlRegEx:=TDIPerlRegEx.Create(nil);
    try
     DIPerlRegEx.MatchPattern:=BenchmarkPatterns[i];
     if DIPerlRegEx.CompileMatchPattern then begin
      t1:=GetTickCount;
      DIPerlRegEx.SetSubjectStr(s);
      if DIPerlRegEx.Match(0)>0 then begin
       while DIPerlRegEx.MatchNext>=0 do begin
       end;
      end;
(*) for j:=0 to sl.Count-1 do begin
      BRRERegExp.Benchmark(sl[j]);
     end;{(**)
      t2:=GetTickCount;
      writeln('/'+BenchmarkPatterns[i]+'/ : ':65,t2-t1:7,' ms');
     end;
    finally
     DIPerlRegEx.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
  writeln('');
  writeln('DIRegEx (DIDFARegEx):');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    DIDFARegEx:=TDIDFARegEx.Create(nil);
    try
     DIDFARegEx.MatchPattern:=BenchmarkPatterns[i];
     if DIDFARegEx.CompileMatchPattern then begin
      t1:=GetTickCount;
      DIDFARegEx.SetSubjectStr(s);
      if DIDFARegEx.Match(0)>0 then begin
       while DIDFARegEx.MatchNext>=0 do begin
       end;
      end;
(*) for j:=0 to sl.Count-1 do begin
      BRRERegExp.Benchmark(sl[j]);
     end;{(**)
      t2:=GetTickCount;
      writeln('/'+BenchmarkPatterns[i]+'/ : ':65,t2-t1:7,' ms');
     end;
    finally
     DIDFARegEx.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
  writeln('');
  writeln('regex.pp:');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    RegExp:=TRegexEngine.Create(BenchmarkPatterns[i]);
    try
     t1:=GetTickCount;
(*)for j:=0 to sl.Count-1 do begin
      h:=0;
      RegExp.MatchString(sl[j],k,h);
     end;{(**)
     k:=0;
     while RegExp.MatchString(s,j,k) do begin
     end;
   //  BRRE.BRREBenchmark(BRRERegExp,s);{}
     t2:=GetTickCount;
     writeln('/'+BenchmarkPatterns[i]+'/ : ':65,t2-t1:7,' ms');
    finally
     RegExp.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
  writeln('');
  writeln('PCRE:');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    PerlRegExInstance:=TPerlRegEx.Create(nil);
    PerlRegExInstance.RegEx:=BenchmarkPatterns[i];
    PerlRegExInstance.Compile;
    try
     t1:=GetTickCount;
(*)for j:=0 to sl.Count-1 do begin
      h:=0;
      RegExp.MatchString(sl[j],k,h);
     end;{(**)
     k:=0;
     PerlRegExInstance.Subject:=s;
     if PerlRegExInstance.Match then begin
      while PerlRegExInstance.MatchAgain do begin
      end;
     end; 
   //  BRRE.BRREBenchmark(BRRERegExp,s);{}
     t2:=GetTickCount;
     writeln('/'+BenchmarkPatterns[i]+'/ : ':65,t2-t1:7,' ms');
    finally
     PerlRegExInstance.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
  writeln('');
  writeln('RegExpr.pas:');
  for i:=low(BenchmarkPatterns) to high(BenchmarkPatterns) do begin
   try
    RegExprInstance:=TRegExpr.Create;
    RegExprInstance.Expression:=BenchmarkPatterns[i];
    RegExprInstance.Compile;
    try
     t1:=GetTickCount;
(*)for j:=0 to sl.Count-1 do begin
      h:=0;
      RegExp.MatchString(sl[j],k,h);
     end;{(**)
     k:=0;
     RegExprInstance.Exec(s);
     while RegExprInstance.ExecNext do begin
     end;
   //  BRRE.BRREBenchmark(BRRERegExp,s);{}
     t2:=GetTickCount;
     writeln('/'+BenchmarkPatterns[i]+'/ : ':65,t2-t1:7,' ms');
    finally
     RegExprInstance.Free;
    end;
   except
    on e:Exception do begin
     writeln(e.Message);
    end;
   end;
  end;
 finally
  sl.Free;
 end;
 s:='';
 readln;
end.


