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
    btnAction: TButton;
    actSeparateColors: TAction;
    actThinStep: TAction;
    actStaircaseRemoval: TAction;
    actFindXAxis: TAction;
    actFindYAxis: TAction;
    actFindFirstPoint: TAction;
    actFindPath: TAction;
    dlgSave: TSaveDialog;
    Button1: TButton;
    actSaveImage: TAction;
    procedure actLoadImageExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actReloadImageExecute(Sender: TObject);
    procedure actSeparateColorsExecute(Sender: TObject);
    procedure actThinStepExecute(Sender: TObject);
    procedure actStaircaseRemovalExecute(Sender: TObject);
    procedure actFindXAxisExecute(Sender: TObject);
    procedure actFindYAxisExecute(Sender: TObject);
    procedure actFindFirstPointExecute(Sender: TObject);
    procedure actFindPathExecute(Sender: TObject);
    procedure actSaveImageExecute(Sender: TObject);
  private
    FImageInterp: TImageInterp;
    FThinCount: integer;
    FFirstPoint: TPoint;
    procedure LoadImage(const aFileName: string);
    procedure Draw(const aImage: TBinaryPixelMap; const aColor: TColor; const aClear: boolean = False); overload;
    procedure Draw(const aImage: TColorPixelMap; const aColor: TColor; const aClear: boolean = False); overload;
    procedure Draw(const aImage: TBinaryPixelMap; const aOriginalImage: TColorPixelMap; const aClear: boolean = False); overload;
    procedure CirclePoints(const aImage: TBinaryPixelMap; const aColor: TColor; const aRadius: integer);
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

procedure TMainForm.actFindFirstPointExecute(Sender: TObject);
var
  vJob: IJob;
begin
  vJob := StartJob;

  Draw(FImageInterp.Colors, $BBBBBB, True);
  Draw(FImageInterp.AllColors, FImageInterp.Colors, False);
  FFirstPoint := FImageInterp.GetFirstPoint;

  // Destaca o início
  imgMain.Canvas.Pen.Color := clLime;
  imgMain.Canvas.Pen.Width := 1;
  imgMain.Canvas.Brush.Style := bsClear;
  imgMain.Canvas.Ellipse(FFirstPoint.X-10, FFirstPoint.Y-10, FFirstPoint.X+10, FFirstPoint.Y+10);

  btnAction.Action := actFindPath;
end;

procedure TMainForm.actFindPathExecute(Sender: TObject);
var
  vJob: IJob;

  vPointList: TPointList;
  i: integer;
  vTst: TStringList;

  vColorStr: string;
begin
  vJob := StartJob;

  Draw(FImageInterp.Colors, $BBBBBB, True);

  vPointList := TImageInterp.FindPath(FImageInterp.AllColors, FImageInterp.Colors,
    FFirstPoint.X, FFirstPoint.Y);
  vTst := TStringList.Create;
  try
    for i := 0 to vPointList.Count - 1 do
    begin
      if vPointList[i].Color = clBlack then
        vColorStr := 'Black'
      else if vPointList[i].Color = clLime then
        vColorStr := 'Green'
      else
        vColorStr := 'None';

      imgMain.Canvas.Pixels[vPointList[i].X, vPointList[i].Y] := vPointList[i].Color;

      vTst.Add(FloatToStr(vPointList[i].X)+','+FloatToStr(vPointList[i].Y)+' ('+IntToStr(Ord(vPointList[i].PathDirection))+') - '+vColorStr);
    end;


    vTst.SaveToFile('f:\teste.txt');
  finally
    vTst.Free;
  end;

  ShellExecute(Handle,'open', 'c:\windows\notepad.exe','f:\teste.txt', nil, SW_SHOWNORMAL);
end;

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
var
  vJob: IJob;
begin
  vJob := StartJob;
  if dlgOpen.Execute then
    LoadImage(dlgOpen.FileName);
end;

procedure TMainForm.actReloadImageExecute(Sender: TObject);
var
  vJob: IJob;
begin
  vJob := StartJob;

  if FileExists(dlgOpen.FileName) then
    LoadImage(dlgOpen.FileName);
end;

procedure TMainForm.actSaveImageExecute(Sender: TObject);
begin
  if dlgSave.Execute then
    imgMain.Picture.SaveToFile(dlgSave.FileName);
end;

procedure TMainForm.actSeparateColorsExecute(Sender: TObject);
var
  vJob: IJob;
begin
  vJob := StartJob;
(*
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


//  Draw(FImageInterp.AllColors, FOriginalImage, 0.5, True);
    vRect.Left := 0;
    vRect.Top := 0;
    vRect.Right := FImageInterp.Width;
    vRect.Bottom := FImageInterp.Height;

    imgMain.Canvas.Brush.Color := clWhite;
    imgMain.Canvas.FillRect(vRect);
                          Tcopymode
  imgMain.Canvas.Draw();
  FOriginalImage.Canvas.CopyMode := cmSrcAnd;
  imgMain.Canvas.CopyRect(vRect, FOriginalImage.Canvas, vRect);

*)
  btnAction.Action := actThinStep;
end;

procedure TMainForm.actStaircaseRemovalExecute(Sender: TObject);
var
  vJob: IJob;

  vRemovedPixels: TBinaryPixelMap;
begin
  vJob := StartJob;

  SetLength(vRemovedPixels, FImageInterp.Width, FImageInterp.Height);
  FImageInterp.RemoveStairCaseAll(vRemovedPixels);
  Draw(vRemovedPixels, clRed);
  Draw(FImageInterp.AllColors, FImageInterp.Colors);

//  CirclePoints(vRemovedPixels, clRed, 5);

  btnAction.Action := actFindFirstPoint;
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
  if not FImageInterp.ThinAllStep(vRemovedPixels) then
    Inc(FThinCount);
  if FImageInterp.ThinState then
    vRemovedColor := clRed
  else
    vRemovedColor := clBlue;

  Draw(vRemovedPixels, vRemovedColor);
  Draw(FImageInterp.AllColors, FImageInterp.Colors);

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

procedure TMainForm.CirclePoints(const aImage: TBinaryPixelMap;
  const aColor: TColor; const aRadius: integer);
var
  x, y : integer;
begin
  imgMain.Canvas.Pen.Color := aColor;
  imgMain.Canvas.Pen.Width := 1;  
  imgMain.Canvas.Brush.Style := bsClear;

  for x := 0 to FImageInterp.Width -1 do
    for y := 0 to FImageInterp.Height -1 do
    begin
      if aImage[x,y] then
        imgMain.Canvas.Ellipse(x-aRadius, y-aRadius, x+aRadius, y+aRadius);
  end;

end;

procedure TMainForm.Draw(const aImage: TBinaryPixelMap;
  const aOriginalImage: TColorPixelMap; const aClear: boolean);
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
        imgMain.Canvas.Pixels[x, y] := aOriginalImage[x, y];
  end;
end;

procedure TMainForm.Draw(const aImage: TColorPixelMap; const aColor: TColor;
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
      if aImage[x,y] <> clWhite then
        imgMain.Canvas.Pixels[x,y] := aColor;
    end;
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

  btnAction.Action := actThinStep;

  FImageInterp.Free;
  FImageInterp := TImageInterp.Create(imgMain);
  FThinCount := 0;

//  imgMain.Canvas.Pixels[0,0] := clBlue;
end;

end.
