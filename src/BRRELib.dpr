library BRRELib;
{$ifdef fpc}
 {$mode delphi}
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

exports BRREGetVersion name 'BRREGetVersion',
        BRREGetVersionString name 'BRREGetVersionString',
        BRRECreate name 'BRRECreate',
        BRRESetMaxMatchLimit name 'BRRESetMaxMatchLimit', 
        BRRESetMaxMatchLimitRecursion name 'BRRESetMaxMatchLimitRecursion',
        BRRESetMaxMatchLimitStackRecursion name 'BRRESetMaxMatchLimitStackRecursion',
        BRRESetCalloutFunc name 'BRRESetCalloutFunc',
        BRRESetCalloutData name 'BRRESetCalloutData',
        BRREGetCountCaptures name 'BRREGetCountCaptures',
        BRREGetNamedGroupIndex name 'BRREGetNamedGroupIndex',
        BRREPrefilterExpression name 'BRREPrefilterExpression',
        BRREPrefilterShortExpression name 'BRREPrefilterShortExpression',
        BRREPrefilterSQLBooleanFullTextExpression name 'BRREPrefilterSQLBooleanFullTextExpression',
        BRREPrefilterSQLExpression name 'BRREPrefilterSQLExpression',
        BRREGetRange name 'BRREGetRange',
        BRREMatch name 'BRREMatch',
        BRREMatchAll name 'BRREMatchAll',
        BRREMatchRef name 'BRREMatchRef',
        BRRETest name 'BRRETest',
        BRREFind name 'BRREFind',
        BRRESplit name 'BRRESplit',
        BRREReplace name 'BRREReplace',
        BRREBenchmark name 'BRREBenchmark',
        BRREFree name 'BRREFree',
        BRREDestroy name 'BRREDestroy';

begin
 InitializeBRRE;
end.
