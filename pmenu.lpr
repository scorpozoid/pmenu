program pmenu;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Dialogs,
  StrUtils,
  Interfaces, // this includes the LCL widgetset
  Forms, fmmain;

{$R *.res}

  function IsVersionRequest: Boolean;
  var
    vParameter: AnsiString;
  begin
    Result := False;
    if (ParamCount > 0) then begin
      vParameter := ParamStr(1);
      Result :=
        ('--version' = vParameter) or
        ('\v' = vParameter)
    end;
  end;

  procedure DisplayVersion;
  begin
    TMainForm.ShowVersion;
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

