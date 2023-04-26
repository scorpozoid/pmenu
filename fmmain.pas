unit fmmain;

interface

uses
  LCLType, Math, Process, // Windows,
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ClosePanel: TPanel;
    SearchEdit: TEdit;
    ItemPanel: TPanel;
    AutoQuitTimer: TTimer;
  private
    FItems: TStringList;
    FCurrentIndex: Integer;

    function GetCurrent: TLabel;
    function GetItem(const aIndex: Integer): TLabel;

    procedure HandleKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
 // procedure HandleKeyPress(aSender: TObject; var aKey: char);
 // procedure HandleKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure HandleCloseClick(aSender: TObject);
    procedure HandleItemClick(aSender: TObject);
    procedure HandleTimer(aSender: TObject);

    procedure HandleSearchEditChange(aSender: TObject);
    procedure HandleSearchKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);

    function InRange(const aIndex: Integer): Boolean;

    function MaxTextHeight: Integer;
    function BuildItemLabel(const aParent: TWinControl; const aLabel: string; const aIndex: Integer): TLabel;

    procedure InitGui;

    procedure InitItems;
    procedure DoneItems;

    function FindConfig: string;
    procedure LoadConfig(const aFileName: TFileName);
    procedure LoadItems(const aStringList: TStringList);

    procedure Refilter(const aText: string);
    function BuildItems: TStringList;

    procedure NextItem;
    procedure PrevItem;

    procedure ActivateCurrent;

    function Return: Integer;
    function Execute: Integer;

    procedure Quit;

    procedure OutputString(const aValue: string);

    property Item[const aIndex: Integer]: TLabel read GetItem;
    property Current: TLabel read GetCurrent;
  public const
    cAppName = 'pmenu';
  public
    class function Version: string;
    class procedure ShowVersion;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure Unused(const aObject: TObject); overload;
begin
  if (aObject <> aObject) then
    Exit;
end;

procedure Unused(const aShiftState: TShiftState); overload;
begin
  if (aShiftState <> aShiftState) then
    Exit;
end;

procedure Unused(const aString: string); overload;
begin
  if (aString <> aString) then
    Exit;
end;


{ TMainForm }

class function TMainForm.Version: string;
begin
  {$I pmenu.version}
end;


class procedure TMainForm.ShowVersion;
begin
  ShowMessage(TMainForm.Version);
  // AllocConsole;
  // IsConsole := True;
  // SysInitStdIO;
  // WriteLn(TMainForm.Version);
end;


constructor TMainForm.Create(aOwner: TComponent);
begin
  FCurrentIndex := -1;
  inherited Create(aOwner);
  FItems := TStringList.Create;
  FItems.OwnsObjects := True;
end;


destructor TMainForm.Destroy;
begin
  if (Assigned(FItems)) then
    FreeAndNil(FItems);
  inherited Destroy;
end;


procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  ShowInTaskbar := stNever;
  Application.Title := TMainForm.cAppName + '-' + TMainForm.Version;
  Application.Hint := Application.Title;
  Caption := Application.Title;
  InitGui;
  InitItems;
  AutoQuitTimer.OnTimer := HandleTimer;
  AutoQuitTimer.Interval := 5000;
  AutoQuitTimer.Enabled := True;
end;


procedure TMainForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  DoneItems;
end;


function TMainForm.MaxTextHeight: Integer;
var
  vText: string;
  vChar: Char;
begin
  Result := 0;
  vText := EmptyStr;
  vChar := 'A';
  while vChar < 'Z' do begin
    vText := vText + vChar;
    vChar := Char(Ord(vChar) + 1)
  end;
  Result := Canvas.TextHeight(vText);
end;


function TMainForm.BuildItemLabel(const aParent: TWinControl; const aLabel: string; const aIndex: Integer): TLabel;

  function ItemLeft: Integer;
  var
    vI: Integer;
  begin
    Result := 0;
    if aParent.ControlCount > 0 then begin
      for vI := 0 to aParent.ControlCount - 1 do begin;
        Result := Max(Result, aParent.Controls[vI].Left + aParent.Controls[vI].Width);
      end;
    end;
  end;

begin
  Result := TLabel.Create(aParent);
  Result.Parent := aParent;
  Result.Transparent := True;
  Result.Caption := aLabel;
  Result.AutoSize := False;
  Result.Font.Color := clGray;
  Result.Align := alNone;
  Result.Top := 0;
  Result.Left := ItemLeft;
  Result.Width := Canvas.TextWidth(Result.Caption) + Canvas.TextWidth('::');
  Result.Height := Self.ClientHeight;
  Result.Left := ItemLeft;
  Result.Tag := aIndex;
  Result.OnClick := HandleItemClick;
  Refresh;
end;


procedure TMainForm.InitGui;
const
  cOffset = 2;
begin
  Color := RGBToColor(3, 3, 3);
  Left := 0;
  Top := 0;
  Height := MaxTextHeight + cOffset;
  Width := Screen.Width;
  FormStyle := fsSystemStayOnTop;
  KeyPreview := True;
  ActiveControl := SearchEdit;

  OnKeyDown := HandleKeyDown;
  OnKeyUp := nil;
  OnKeyPress := nil;

  SearchEdit.Font.Color := clWhite;
  SearchEdit.Font.Style := SearchEdit.Font.Style + [fsBold];
  SearchEdit.Width := Screen.Width div 10;
  SearchEdit.OnChange := HandleSearchEditChange;
  SearchEdit.OnKeyDown := HandleSearchKeyDown;

  ClosePanel.OnClick := HandleCloseClick;

  Visible := True;
end;


procedure TMainForm.InitItems;
var
  vConfig: TFileName;
  vItems: TStringList;
begin
  vConfig := FindConfig;
  if (FileExists(vConfig)) then begin
    LoadConfig(vConfig);
  end else begin
    vItems := BuildItems;
    if (Assigned(vItems)) then try
      LoadItems(vItems);
    finally
      FreeAndNil(vItems)
    end;
  end;
  Refilter(EmptyStr);
end;


procedure TMainForm.DoneItems;
begin
  if (1 > FItems.Count) then
    Exit;
  // ! OwnsObjects
  // for vI := FItems.Count - 1 downto 0 do begin
  //   vObject := FItems.Objects[vI];
  //   if (Assigned(vObject)) then try
  //     if (vObject is TLabel) then begin
  //       vLabel := vObject as TLabel;
  //       vLabel.Hide;
  //       vLabel.Parent := nil;
  //     end;
  //     FreeAndNil(vObject);
  //   finally
  //     FItems.Objects[vI] := nil;
  //   end;
  // end;
  FItems.Clear;
end;


function TMainForm.FindConfig: string;
var
  vParameter: string;
begin
  Result := ExtractFileDir(Application.ExeName) + '\pmenu.menu';
  if (not FileExists(Result)) then
    Result := ExtractFileDir(Application.ExeName) + '\example.pmenu';
  if (ParamCount > 0) then begin
    vParameter := ParamStr(1);
    if (FileExists(vParameter)) then begin
      Result := vParameter;
    end;
  end;
end;


procedure TMainForm.LoadConfig(const aFileName: TFileName);
var
  vStringList: TStringList;
begin
  if (not FileExists(aFileName)) then
    Exit;
  vStringList := TStringList.Create;
  try
    vStringList.LoadFromFile(aFileName);
    LoadItems(vStringList);
  finally
    vStringList.Free;
  end;
end;


procedure TMainForm.LoadItems(const aStringList: TStringList);
var
  vI: Integer;
  vLabel: TLabel;
  vValue: string;
  vCaption: string;
  vAlreadyExists: Boolean;

  function IsCommented(const aLine: string): Boolean;
  begin
    Result := aLine.StartsWith('#') or aLine.StartsWith('//') or aLine.StartsWith('--');
  end;

begin
  if (not Assigned(aStringList)) then
    Exit;
  if (aStringList.Count < 1) then
    Exit;
  for vI := 0 to aStringList.Count - 1 do begin
    vValue := Trim(aStringList.Strings[vI]);
    vCaption := Trim(aStringList.Names[vI]);
    if (EmptyStr = vCaption) then
      Continue;
    if (IsCommented(vCaption)) then
      Continue;
    vAlreadyExists := -1 < FItems.IndexOf(vCaption);
    if (vAlreadyExists) then
      Continue;
    vLabel := BuildItemLabel(ItemPanel, vCaption, vI);
    if (not Assigned(vLabel)) then
      Continue;
    FItems.AddObject(vValue, vLabel);
  end;
end;


procedure TMainForm.Refilter(const aText: string);
var
  vI: Integer;
  vCaption: string;
  vCurrent: TLabel;
  vOffset: Integer;
begin
  vOffset := Canvas.TextWidth('::');
  FCurrentIndex := -1;
  for vI := 0 to FItems.Count - 1 do begin
    vCurrent := Item[vI];
    if (not Assigned(vCurrent)) then
      Continue;
    if (aText = EmptyStr) then begin
      vCurrent.Visible := True;
    end else begin
      vCaption := vCurrent.Caption;
      vCurrent.Visible := vCaption.Contains(aText);
    end;
    if (vCurrent.Visible) then begin
      if (0 > FCurrentIndex) then
        FCurrentIndex := vI;
      vCurrent.Left := vOffset;
      vOffset := Max(vOffset, vCurrent.Left + vCurrent.Width + Canvas.TextWidth('::'));
    end;
  end;
  ActivateCurrent;
end;


function TMainForm.BuildItems: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('exit');
  Result.Add('--version');
end;


procedure TMainForm.NextItem;
var
  vIndex: Integer;
  vItem: TLabel;
begin
  if (1 > FItems.Count) then
    Exit;
  vIndex := FCurrentIndex;
  while vIndex < FItems.Count do begin
    vIndex := Max(0, Min(vIndex + 1, FItems.Count));
    vItem := Item[vIndex];
    if (not Assigned(vItem)) then
      Continue;
    if (vItem.Visible) then begin
      FCurrentIndex := vIndex;
      Break;
    end;
  end;
  ActivateCurrent;
end;


procedure TMainForm.PrevItem;
var
  vIndex: Integer;
  vItem: TLabel;
begin
  if (1 > FItems.Count) then
    Exit;
  vIndex := FCurrentIndex;
  while vIndex > 0 do begin
    vIndex := Max(-1, Min(vIndex - 1, FItems.Count - 1));
    vItem := Item[vIndex];
    if (not Assigned(vItem)) then
      Continue;
    if (vItem.Visible) then begin
      FCurrentIndex := vIndex;
      Break;
    end;
  end;
  ActivateCurrent;
end;


function TMainForm.Return: Integer;
var
  vResult: string;
  vCurrent: TLabel;
begin
  Result := -1;
  vCurrent := Current;
  if (Assigned(vCurrent)) then begin
    Result := FCurrentIndex;
    vResult := vCurrent.Caption;
    Execute;
    OutputString(vResult);
  end;
  Quit;
end;


function TMainForm.Execute: Integer;
var
  vCommand: string;
  vOut: string;
  vUserName: string;
  vProcessOptions: TProcessOptions;
  vWindowOptions: TShowWindowOptions;
begin
  Result := -1;
  if (InRange(FCurrentIndex)) then
    vCommand := Trim(FItems.ValueFromIndex[FCurrentIndex]);
  if (EmptyStr = vCommand) then
    Exit;
  vUserName := GetEnvironmentVariable('USERNAME');
  vCommand := ReplaceText(vCommand, '%USERNAME%', vUserName);
  vOut := EmptyStr;
  vProcessOptions := [poDetached]; // poDetached
  vWindowOptions := swoNone; // swoMinimize;
  try
    // OutputString('Execute: ' + vCommand);
    RunCommand(vCommand, [], vOut, vProcessOptions, vWindowOptions);
    // vExecuteFlags: TExecuteFlags;
    // vExecuteFlags := [];
    // ExecuteProcess(vCommand, '', vExecuteFlags);
  except
    on E: Exception do
      OutputString(vCommand + ' -- ' + E.Message);
  end;
end;


procedure TMainForm.Quit;
begin
  Close; // Application.Terminate;
end;


procedure TMainForm.OutputString(const aValue: string);
begin
  Unused(aValue);
  // WriteLn(aValue);
end;


procedure TMainForm.ActivateCurrent;
var
  vI: Integer;
  vItem: TLabel;
  vCurrent: TLabel;
  vOverdoze, vRightSide: Integer;
begin
  vCurrent := nil;
  for vI := 0 to FItems.Count - 1 do begin
    vItem := Item[vI];
    if (not Assigned(vItem)) then
      Continue;
    if (vI = FCurrentIndex) then begin
      vItem.Font.Color := clWhite;
      vCurrent := vItem;
    end else begin
      vItem.Font.Color := clGray;
    end;
  end;
  if (not Assigned(vCurrent)) then
    Exit;
  if (vCurrent.Left < 0) then begin
    vOverdoze := 0 - vCurrent.Left;
    for vI := 0 to FItems.Count - 1 do begin
      vItem := Item[vI];
      if (not Assigned(vItem)) then
        Continue;
      vItem.Left := vItem.Left + vOverdoze;
    end;
  end else begin
    vRightSide := ClosePanel.Width + ItemPanel.Left + vCurrent.Left + vCurrent.Width;
    vOverdoze := vRightSide - Screen.Width;
    if (vOverdoze > 0) then begin
      for vI := 0 to FItems.Count - 1 do begin
        vItem := Item[vI];
        if (not Assigned(vItem)) then
          Continue;
        vItem.Left := vItem.Left - vOverdoze;
      end;
    end;
  end;
end;


function TMainForm.GetCurrent: TLabel;
begin
  Result := GetItem(FCurrentIndex);
end;


function TMainForm.GetItem(const aIndex: Integer): TLabel;
begin
  Result := nil;
  if (not InRange(aIndex)) then
    Exit;
  if (not Assigned(FItems.Objects[aIndex])) then
    Exit;
  if (not (FItems.Objects[aIndex] is TLabel)) then
    Exit;
  Result := FItems.Objects[aIndex] as TLabel;
end;


procedure TMainForm.HandleSearchKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  Unused(aSender);
  Unused(aShift);
  AutoQuitTimer.Enabled := False;
  if (aKey = VK_RIGHT) then
    aKey := 0;
  if (aKey = VK_LEFT) then
    aKey := 0;
  AutoQuitTimer.Enabled := True;
end;


procedure TMainForm.HandleKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  Unused(aSender);
  Unused(aShift);
  AutoQuitTimer.Enabled := False;
  if (aKey = VK_ESCAPE) then
    Quit;
  if (aKey = VK_RETURN) then
    Return;
  if (aKey = VK_RIGHT) then
    NextItem;
  if (aKey = VK_LEFT) then
    PrevItem;
  AutoQuitTimer.Enabled := True;
end;


procedure TMainForm.HandleCloseClick(aSender: TObject);
begin
  if (aSender is TLabel) then
    Quit;
end;


procedure TMainForm.HandleItemClick(aSender: TObject);
var
  vItemTag: Integer;
begin
  if (aSender is TLabel) then begin
    SearchEdit.Text := (aSender as TLabel).Caption;
    Refilter(SearchEdit.Text);
    vItemTag := (aSender as TLabel).Tag;
    if (InRange(vItemTag)) then
      FCurrentIndex := vItemTag;
    Return;
  end;
end;


procedure TMainForm.HandleTimer(aSender: TObject);
begin
  Unused(aSender);
  Quit;
end;


procedure TMainForm.HandleSearchEditChange(aSender: TObject);
var
  vEdit: TEdit;
  vWidth: Integer;
begin
  if (aSender is TEdit) then begin
    vEdit := aSender as TEdit;
    vWidth := Integer(Round(1.25 * Self.Canvas.TextWidth(vEdit.Text)));
    vEdit.Width := Min(Max(Screen.Width div 12, vWidth), Screen.Width div 2);
    Refilter(vEdit.Text);
    if (vEdit.Text = '--version') then
      ShowVersion;
  end;
end;


function TMainForm.InRange(const aIndex: Integer): Boolean;
begin
  Result := Assigned(FItems);
  if (not Result) then
    Exit;
  Result := 0 < FItems.Count;
  if (not Result) then
    Exit;
  Result := (aIndex > -1) and (aIndex < FItems.Count);
end;


end.

