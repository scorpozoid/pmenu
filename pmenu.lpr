program pmenu;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Windows,
  StrUtils,
  Interfaces, // this includes the LCL widgetset
  Forms, fmmain;

{$R *.res}

  function IsVersionRequest: Boolean;
  var
    vParameter: Ansistring;
  begin
    Result := False;
    if (ParamCount > 0) then begin
      vParameter := ParamStr(1);
      Result :=
        MatchText('--version', [vParameter]) or
        MatchText('\v', [vParameter])
    end;
  end;

  procedure DisplayVersion;
  begin
    AllocConsole;
    IsConsole := True;
    SysInitStdIO;
    WriteLn(TMainForm.Version);
  end;

begin
  if (IsVersionRequest) then begin
    DisplayVersion;
    Exit;
  end;

  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

