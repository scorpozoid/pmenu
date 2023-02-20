unit fmmain;

interface

uses
  // Windows,
  LCLType, Math,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ClosePanel: TPanel;
    SearchEdit: TEdit;
    ItemPanel: TPanel;
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

    procedure HandleSearchEditChange(aSender: TObject);
    procedure HandleSearchKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);

    function InRange(const aIndex: Integer): Boolean;

    function MaxTextHeight: Integer;
    function BuildItemLabel(const aParent: TWinControl; const aLabel: string; const aIndex: Integer): TLabel;

    procedure InitGui;

    procedure InitItems;
    procedure DoneItems;

    procedure LoadItems(const aStringList: TStringList);

    procedure Refilter(const aText: string);
    function BuildItems: TStringList;

    procedure NextItem;
    procedure PrevItem;
    function Return: Integer;
    procedure ActivateCurrent;

    property Item[const aIndex: Integer]: TLabel read GetItem;
    property Current: TLabel read GetCurrent;
  public const
    cAppName = 'dmeenu';
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

constructor TMainForm.Create(aOwner: TComponent);
begin
  // AllocConsole;
  // IsConsole := True;
  // SysInitStdIO;

  Visible := False;
  inherited Create(aOwner);
  FCurrentIndex := -1;
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
  InitGui;
  InitItems;
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
  vItems: TStringList;
begin
  vItems := BuildItems;
  if (Assigned(vItems)) then try
    LoadItems(vItems);
  finally
    FreeAndNil(vItems)
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


procedure TMainForm.LoadItems(const aStringList: TStringList);
var
  vI: Integer;
  vLabel: TLabel;
  vCaption: string;
  vAlreadyExists: Boolean;
begin
  if (not Assigned(aStringList)) then
    Exit;
  if (aStringList.Count < 1) then
    Exit;
  for vI := 0 to aStringList.Count - 1 do begin
    vCaption := Trim(aStringList.Strings[vI]);
    if (EmptyStr = vCaption) then
      Continue;
    vAlreadyExists := -1 < FItems.IndexOf(vCaption);
    if (vAlreadyExists) then
      Continue;
    vLabel := BuildItemLabel(ItemPanel, vCaption, vI);
    if (not Assigned(vLabel)) then
      Continue;
    FItems.AddObject(vCaption, vLabel);
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
  Result.Add('label1');
  Result.Add('ff');
  Result.Add('far');
  Result.Add('label2');
  Result.Add('ooo'); Result.Add('ooo2'); Result.Add('ooo3'); Result.Add('ooo4');
  Result.Add('label23'); Result.Add('label25'); Result.Add('label26'); Result.Add('label27');
  Result.Add('label33'); Result.Add('label35'); Result.Add('label36'); Result.Add('label37');
  Result.Add('label43'); Result.Add('label45'); Result.Add('label46'); Result.Add('label47');
  Result.Add('label53'); Result.Add('label55'); Result.Add('label56'); Result.Add('label57');
  Result.Add('label63'); Result.Add('label65'); Result.Add('label66'); Result.Add('label67');
  Result.Add('label73'); Result.Add('label75'); Result.Add('label76'); Result.Add('label77');
  Result.Add('label83'); Result.Add('label85'); Result.Add('label86'); Result.Add('label87');
  Result.Add('ooo111'); Result.Add('ooo2222'); Result.Add('ooo3333'); Result.Add('ooo4444');
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
    Writeln(vResult);
  end;
  Application.Terminate;
end;


procedure TMainForm.ActivateCurrent;
var
  vI: Integer;
  vItem: TLabel;
  vCurrent: TLabel;
  vOverdoze, vRightSide: Integer;
begin
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
  if (aKey = VK_RIGHT) then
    aKey := 0;
  if (aKey = VK_LEFT) then
    aKey := 0;
  if (aSender <> aSender) then
    Exit;
  if (aShift <> aShift) then
    Exit;
end;


procedure TMainForm.HandleKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  if (aKey = VK_ESCAPE) then
    Application.Terminate;
  if (aKey = VK_RETURN) then
    Return;
  if (aKey = VK_RIGHT) then
    NextItem;
  if (aKey = VK_LEFT) then
    PrevItem;
  if (aSender <> aSender) then
    Exit;
  if (aShift <> aShift) then
    Exit;
end;


procedure TMainForm.HandleCloseClick(aSender: TObject);
begin
  if (aSender is TLabel) then
    Application.Terminate;
end;


procedure TMainForm.HandleItemClick(aSender: TObject);
begin
  if (aSender is TLabel) then
    Return;
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

