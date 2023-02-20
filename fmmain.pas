unit fmmain;

interface

uses
  Windows,
  LCLType, Math,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
  private
    // FMaxTextHeight: Integer;
    FItems: TStringList;
    FCurrent: Integer;
    procedure HandleKey(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure HandleSearchKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure HandleKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure HandleKeyPress(aSender: TObject; var aKey: char);
    procedure HandleKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure HandleCloseClick(aSender: TObject);
    procedure HandleItemClick(aSender: TObject);
    procedure HandleEditChange(aSender: TObject);

    function InRange(const aIndex: Integer): Boolean;
    function MaxTextHeight: Integer;
    procedure SetupPosition;
    function BuildSearchEdit(const aParent: TWinControl): TEdit;
    function BuildCloseLabel(const aParent: TWinControl): TLabel;
    function BuildItemLabel(const aParent: TWinControl; const aLabel: string): TLabel;
    procedure BuildDefaultItems(const aParent: TWinControl);
    procedure BuildMenuItems(const aParent: TWinControl);
    procedure LoadItems(const aStringList: TStringList);

    procedure Refilter(const aText: string);
    function BuildItems: TStringList;

    procedure NextItem;
    procedure PrevItem;
    function Return: Integer;
    procedure ActivateCurrent;
  public const
    cAppName = 'dmeenu';
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

constructor TMainForm.Create(aOwner: TComponent);
begin
  Visible := False;
  inherited Create(aOwner);
  FCurrent := -1;
  FItems := TStringList.Create;
  KeyPreview := True;
  OnKeyDown := HandleKeyDown;
  OnKeyUp := HandleKeyUp;
  OnKeyPress := HandleKeyPress;
  SetupPosition;
  BuildDefaultItems(Self);
  BuildMenuItems(Self);
end;


destructor TMainForm.Destroy;
begin
  if (Assigned(FItems)) then
    FreeAndNil(FItems);
  inherited Destroy;
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


procedure TMainForm.SetupPosition;
const
  cOffset = 2;
begin
  Color := RGBToColor(3,3,3); // clDkGray;
  Left := 0;
  Top := 0;
  Height := MaxTextHeight + cOffset;
  Width := Screen.Width;
  KeyPreview := True;
  FormStyle := fsSystemStayOnTop;
end;


function TMainForm.BuildSearchEdit(const aParent: TWinControl): TEdit;
begin
  Result := TEdit.Create(aParent);
  Result.Left := 0;
  Result.Parent := aParent;
  Result.DoubleBuffered := True;
  Result.Align := alLeft;
  Result.Alignment := taLeftJustify;
  Result.BorderStyle := bsNone;
  Result.Color := aParent.Color;
  Result.Font.Color := clWhite;
  Result.Font.Style := Result.Font.Style + [fsBold];
  Result.Width := Screen.Width div 10; // Canvas.TextWidth(cAppName);
  Result.Visible := True;
  Result.OnChange := HandleEditChange;
  Result.OnKeyDown := HandleSearchKeyDown;
end;


function TMainForm.BuildCloseLabel(const aParent: TWinControl): TLabel;
begin
  Result := TLabel.Create(aParent);
  Result.Parent := aParent;
  // Result.Color := clGreen;
  Result.Transparent := True;
  Result.Caption := 'X';
  Result.Align := alRight;
  Result.AutoSize := True;
  Result.Font.Color:= clRed;
  Result.Font.Style := Result.Font.Style + [fsBold];
  Result.Width := Canvas.TextWidth(Result.Caption) + 100;
  Result.OnClick := HandleCloseClick;
//Result.onke := HandleKeyDown;
  Result.Visible := True;
end;


function TMainForm.BuildItemLabel(const aParent: TWinControl; const aLabel: string): TLabel;

  function ItemLeft: Integer;
  var
    vI: Integer;
  begin
    Result := 0;
    if aParent.ControlCount > 0 then begin
      for vI := 0 to aParent.ControlCount - 1 do begin;
        Result := Max(Result, aParent.Controls[vI].Left + aParent.Controls[vI].Width);
      end;
//      Result := Result + aParent.Controls[aParent.ControlCount - 1].Width;;
    end;
  end;

begin
  Result := TLabel.Create(aParent);
  Result.Parent := aParent;
  Result.Transparent := True;
  Result.Caption := aLabel;
  Result.AutoSize := False;
  Result.Font.Color := clGray;
  Result.Width := Canvas.TextWidth(Result.Caption) + 32;
  Result.Top := 0;
  Result.Left := ItemLeft; // Screen.Width;
  Result.Name := 'Label' + IntToStr(ItemLeft);
  Result.Align := alLeft;
//  Result.Visible := True;
  Result.OnClick := HandleItemClick;
  Refresh;
end;


procedure TMainForm.BuildDefaultItems(const aParent: TWinControl);
var
  vEdit: TEdit;
begin
  vEdit := BuildSearchEdit(aParent);
  BuildCloseLabel(aParent);
  Visible := True;
  ActiveControl := vEdit;
end;


procedure TMainForm.BuildMenuItems(const aParent: TWinControl);
var
  vItems: TStringList;
begin
  vItems := BuildItems;
  if (Assigned(vItems)) then try
    LoadItems(vItems);
  finally
    FreeAndNil(vItems)
  end;
end;


procedure TMainForm.LoadItems(const aStringList: TStringList);
var
  vI: Integer;
  vLabel: TLabel;
  vCaption: string;
begin
  if (not Assigned(aStringList)) then
    Exit;
  if (aStringList.Count < 1) then
    Exit;
  for vI := 0 to aStringList.Count - 1 do begin
    vCaption := aStringList.Strings[vI];
    vLabel := BuildItemLabel(Self, vCaption);
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
  vOffset := 1;
  for vI := 0 to FItems.Count - 1 do begin
    if (not Assigned(FItems.Objects[vI])) then
      Continue;
    if (not (FItems.Objects[vI] is TLabel)) then
      Continue;
    vCurrent := FItems.Objects[vI] as TLabel;
    if (aText = EmptyStr) then begin
      vCurrent.Visible := True;
    end else begin
      vCaption := vCurrent.Caption;
      vCurrent.Visible := vCaption.Contains(aText);
    end;
    if (vCurrent.Visible) then begin
      vCurrent.Left := vOffset;
      vOffset := Max(vOffset, vCurrent.Left + vCurrent.Width);
    end;
  end;
end;


function TMainForm.BuildItems: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('label1');
  Result.Add('ff');
  Result.Add('far');
  Result.Add('label2');
  Result.Add('ooo');
end;


procedure TMainForm.NextItem;
begin
  if (FItems.Count > 0) then
    FCurrent := Min(FCurrent + 1, FItems.Count - 1)
  else
    FCurrent := -1;
  ActivateCurrent;
end;


procedure TMainForm.PrevItem;
begin
  if (FItems.Count > 0) then
    FCurrent := Max(0, FCurrent - 1)
  else
    FCurrent := -1;
  ActivateCurrent;
end;


function TMainForm.Return: Integer;
var
  vResult: string;
  vCurrent: TLabel;
begin
  Result := -1;
  if InRange(FCurrent) then
    Exit;
  if (not Assigned(FItems.Objects[FCurrent])) then
    Exit;
  if (not (FItems.Objects[FCurrent] is TLabel)) then
    Exit;
  Result := FCurrent;
  vCurrent := FItems.Objects[Result] as TLabel;
  vResult := vCurrent.Caption;
  Writeln(vResult);
  Application.Terminate;
end;


procedure TMainForm.ActivateCurrent;
var
  vI: Integer;
  vCurrent: TLabel;
begin
  vCurrent := nil;
  for vI := 0 to FItems.Count - 1 do begin
    if (not Assigned(FItems.Objects[vI])) then
      Exit;
    if (not (FItems.Objects[vI] is TLabel)) then
      Exit;
    vCurrent := FItems.Objects[vI] as TLabel;
    if (vI = FCurrent) then
      vCurrent.Font.Color := clWhite
    else
      vCurrent.Font.Color := clGray;
  end;
end;


procedure TMainForm.HandleKey(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  if (aKey = VK_ESCAPE) then
    Application.Terminate;
  if (aKey = VK_RETURN) then
    Return;
  if (aKey = VK_RIGHT) then
    NextItem;
  if (aKey = VK_LEFT) then
    PrevItem;
end;

procedure TMainForm.HandleSearchKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  if (aKey = VK_RIGHT) then
    aKey := 0;
  if (aKey = VK_LEFT) then
    aKey := 0;
end;


procedure TMainForm.HandleKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  HandleKey(aSender, aKey, aShift);
end;


procedure TMainForm.HandleKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  // HandleKey(aSender, aKey, aShift);
end;


procedure TMainForm.HandleCloseClick(aSender: TObject);
begin
  Application.Terminate;
end;


procedure TMainForm.HandleItemClick(aSender: TObject);
begin
  // MessageDlg('1', '2', dlg);
end;


procedure TMainForm.HandleEditChange(aSender: TObject);
var
  vEdit: TEdit;
  vWidth: Integer;
const
  cOffset = 36;
begin
  if (aSender is TEdit) then begin
    vEdit := aSender as TEdit;
    vWidth := Self.Canvas.TextWidth(vEdit.Text) + vEdit.BorderWidth * 2 + cOffset;
    if (vWidth > vEdit.Width) then begin
      if (vWidth < (Screen.Width div 2)) then begin
        vEdit.Width := vWidth + Round(0.25 * vWidth);
      end;
    end;
    vEdit.Left := 0;
    Refilter(vEdit.Text);
  end;
end;

function TMainForm.InRange(const aIndex: Integer): Boolean;
begin
  Result := Assigned(FItems);
  if not Result then
    Exit;
  Result := 0 < FItems.Count;
  if not Result then
    Exit;
  Result := not ((FCurrent > -1) and (FCurrent < FItems.Count - 1));
end;

procedure TMainForm.HandleKeyPress(aSender: TObject; var aKey: char);
begin
end;

end.

