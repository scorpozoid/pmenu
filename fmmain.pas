unit fmmain;

interface

uses
  LCLType, StdCtrls,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
  private
    // FMaxTextHeight: Integer;
    procedure HandleKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure HandleKeyPress(aSender: TObject; var aKey: char);
    procedure HandleKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
    procedure HandleCloseClick(aSender: TObject);
    procedure HandleItemClick(aSender: TObject);
    procedure HandleEditChange(aSender: TObject);

    function MaxTextHeight: Integer;
    procedure SetupPosition;
    function BuildSearchEdit(const aParent: TWinControl): TEdit;
    function BuildCloseLabel(const aParent: TWinControl): TLabel;
    function BuildItemLabel(const aParent: TWinControl; const aLabel: string): TLabel;
    procedure BuildDefaultItems(const aParent: TWinControl);
    procedure LoadItems(const aStringList: TStringList);

    function BuildItems: TStringList;
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
  OnKeyDown := HandleKeyDown;
  OnKeyUp := HandleKeyUp;
  OnKeyPress := HandleKeyPress;
  SetupPosition;
  BuildDefaultItems(Self);
end;


destructor TMainForm.Destroy;
begin
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
  Color := clDkGray;
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
  Result.Width := Canvas.TextWidth(cAppName);
  Result.Visible := True;
  Result.OnChange := HandleEditChange;
end;


function TMainForm.BuildCloseLabel(const aParent: TWinControl): TLabel;
begin
  Result := TLabel.Create(aParent);
  Result.Color := clGreen;
//Result.Transparent := True;
  Result.Caption := 'X';
  Result.Align := alLeft;
  Result.AutoSize := True;
  Result.Font.Color:= clRed;
  Result.Font.Style := Result.Font.Style + [fsBold];
  Result.Width := Canvas.TextWidth(Result.Caption) + 100;
  Result.OnClick := HandleCloseClick;
  Result.Visible := True;
end;


function TMainForm.BuildItemLabel(const aParent: TWinControl; const aLabel: string): TLabel;
begin
  Result := TLabel.Create(aParent);
  Result.Color := clGreen;
//Result.Transparent := True;
  Result.Caption := aLabel;
  Result.Align := alLeft;
  Result.AutoSize := True;
  Result.Font.Color := clWhite;
  Result.Font.Style := Result.Font.Style + [fsBold];
  Result.Width := Canvas.TextWidth(Result.Caption) + 32;
  Result.OnClick := HandleItemClick;
  Result.Visible := True;
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


procedure TMainForm.LoadItems(const aStringList: TStringList);
var
  vI: Integer;
begin
  if (not Assigned(aStringList)) then
    Exit;
  if (aStringList.Count < 1) then
    Exit;
  for vI := 0 to aStringList.Count - 1 do begin
    BuildItemLabel(Self, aStringList.Strings[vI]);
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

procedure TMainForm.HandleKeyDown(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  if (aKey = VK_ESCAPE) then
     Application.Terminate;
end;

procedure TMainForm.HandleKeyUp(aSender: TObject; var aKey: Word; aShift: TShiftState);
begin
  if (aKey = VK_ESCAPE) then
     Application.Terminate;
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
      if (vWidth < (Screen.Width div 5)) then begin
        vEdit.Width := vWidth + Round(0.25 * vWidth);
      end;
    end;
    vEdit.Left := 0;
  end;
end;

procedure TMainForm.HandleKeyPress(aSender: TObject; var aKey: char);
begin
end;

end.

