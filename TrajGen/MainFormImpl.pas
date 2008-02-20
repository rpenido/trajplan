unit MainFormImpl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ExtCtrls, ImageInterpImpl, XPMan;

type
  TMainForm = class(TForm)
    imgMain: TImage;
    btnLoadImage: TButton;
    aclMain: TActionList;
    actLoadImage: TAction;
    dlgOpen: TOpenDialog;
    btnReloadImage: TButton;
    actReloadImage: TAction;
    imgBlack: TImage;
    imgBlue: TImage;
    imgRed: TImage;
    btnAction: TButton;
    imgGreen: TImage;
    actSeparateColors: TAction;
    actThinStep: TAction;
    actStaircaseRemoval: TAction;
    actFindXAxis: TAction;
    actFindYAxis: TAction;
    procedure actLoadImageExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actReloadImageExecute(Sender: TObject);
    procedure actSeparateColorsExecute(Sender: TObject);
    procedure actThinStepExecute(Sender: TObject);
    procedure actStaircaseRemovalExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure actFindXAxisExecute(Sender: TObject);
    procedure actFindYAxisExecute(Sender: TObject);
  private
    FImageInterp: TImageInterp;
    FThinCount: integer;
    procedure LoadImage(const aFileName: string);
    procedure Draw(const aImage: TBinaryPixelMap; const aColor: TColor; const aClear: boolean = False);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  ProgUtilsImpl, Types, ShellAPI;
{$R *.dfm}

procedure TMainForm.actFindXAxisExecute(Sender: TObject);
var
  a, b: double;
  x, y: integer;
begin
  FImageInterp.FindXAxis(a, b);
  x := 0;
  y := Round(a*x + b);

  imgMain.Canvas.Pen.Color := clRed;

  imgMain.Canvas.MoveTo(x, y);

  x := FImageInterp.Width;
  y := Round(a*x + b);

  imgMain.Canvas.LineTo(x, y);

  btnAction.Action := actFindYAxis;
end;

procedure TMainForm.actFindYAxisExecute(Sender: TObject);
var
  a, b: double;
  x, y: integer;
begin
  FImageInterp.FindYAxis(a, b);
  x := 0;
  y := Round(a*x + b);

  imgMain.Canvas.Pen.Color := clLime;

  imgMain.Canvas.MoveTo(x, y);

  x := FImageInterp.Width;
  y := Round(a*x + b);

  imgMain.Canvas.LineTo(x, y);
end;

procedure TMainForm.actLoadImageExecute(Sender: TObject);
begin
  if dlgOpen.Execute then
    LoadImage(dlgOpen.FileName);
end;

procedure TMainForm.actReloadImageExecute(Sender: TObject);
begin
  if FileExists(dlgOpen.FileName) then
    LoadImage(dlgOpen.FileName);
end;

procedure TMainForm.actSeparateColorsExecute(Sender: TObject);
var
  vJob: IJob;
begin
  vJob := StartJob;

  TImageInterp.Draw(imgBlack, FImageInterp.Width, FImageInterp.Height,
    FImageInterp.Black, clBlack);

  TImageInterp.Draw(imgRed, FImageInterp.Width, FImageInterp.Height,
    FImageInterp.Red, clRed);

  TImageInterp.Draw(imgGreen, FImageInterp.Width, FImageInterp.Height,
    FImageInterp.Green, clLime);

  TImageInterp.Draw(imgBlue, FImageInterp.Width, FImageInterp.Height,
    FImageInterp.Blue, clBlue);

  Draw(FImageInterp.Black, $000000, True);
  Draw(FImageInterp.Red, $BBBBFF);
  Draw(FImageInterp.Green, $BBFFBB);
  Draw(FImageInterp.Blue, $FFBBBB);

  btnAction.Action := actThinStep;
end;

procedure TMainForm.actStaircaseRemovalExecute(Sender: TObject);
var
  vJob: IJob;

  vRemovedPixels: TBinaryPixelMap;
begin
  vJob := StartJob;

  SetLength(vRemovedPixels, FImageInterp.Width, FImageInterp.Height);
  FImageInterp.RemoveStairCaseBlack(vRemovedPixels);
  Draw(vRemovedPixels, clRed);
  Draw(FImageInterp.Black, clBlack);

  btnAction.Action := actFindXAxis;
end;

procedure TMainForm.actThinStepExecute(Sender: TObject);
var
  vJob: IJob;

  vRemovedPixels: TBinaryPixelMap;
  vRemovedColor: TColor;
begin
  vJob := StartJob;

  Draw(FImageInterp.LastRemovedPixels, $BBBBBB);
  SetLength(vRemovedPixels, FImageInterp.Width, FImageInterp.Height);
  if not FImageInterp.ThinBlackStep(vRemovedPixels) then
    Inc(FThinCount);
  if FImageInterp.ThinState then
    vRemovedColor := clRed
  else
    vRemovedColor := clLime;

  Draw(vRemovedPixels, vRemovedColor);
  Draw(FImageInterp.Black, clBlack);

  if FThinCount > 1 then
    btnAction.Action := actStaircaseRemoval;
end;

procedure TMainForm.AfterConstruction;
begin
  inherited;
  FImageInterp := TImageInterp.Create(imgMain);
end;

procedure TMainForm.BeforeDestruction;
begin
  inherited;
  FImageInterp.Free;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  vPointList: TPointList;
  i: integer;
  vTst: TStringList;
begin
  vPointList := TImageInterp.FindPath(FImageInterp.Black, 0, 0);
  vTst := TStringList.Create;
  try
    for i := 0 to vPointList.Count - 1 do
      vTst.Add(FloatToStr(vPointList[i].X)+','+FloatToStr(vPointList[i].Y)+' ('+IntToStr(Ord(vPointList[i].PathDirection))+')');

    vTst.SaveToFile('f:\teste.txt');
  finally
    vTst.Free;
  end;

  ShellExecute(Handle,'open', 'c:\windows\notepad.exe','f:\teste.txt', nil, SW_SHOWNORMAL);
end;

procedure TMainForm.Draw(const aImage: TBinaryPixelMap; const aColor: TColor;
  const aClear: boolean);
var
  vRect: TRect;

  x,y: integer;
begin
  if aClear then
  begin
    vRect.Left := 0;
    vRect.Top := 0;
    vRect.Right := FImageInterp.Width;
    vRect.Bottom := FImageInterp.Height;

    imgMain.Canvas.Brush.Color := clWhite;
    imgMain.Canvas.FillRect(vRect);
  end;

  for x := 0 to FImageInterp.Width -1 do
    for y := 0 to FImageInterp.Height -1 do
    begin
      if aImage[x,y] then
        imgMain.Canvas.Pixels[x,y] := aColor;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  vDefaultImageName: string;
begin
  vDefaultImageName := ExtractFilePath(ParamStr(0))+'teste-cam.bmp';
  if FileExists(vDefaultImageName) then
  begin
    imgMain.Picture.LoadFromFile(vDefaultImageName);
    dlgOpen.FileName := vDefaultImageName;
  end;
end;

procedure TMainForm.LoadImage(const aFileName: string);
begin
  imgMain.Picture.LoadFromFile(aFileName);
  btnAction.Action := actSeparateColors;

  FImageInterp.Free;
  FImageInterp := TImageInterp.Create(imgMain);
end;

end.
