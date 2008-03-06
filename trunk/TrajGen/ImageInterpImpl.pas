unit ImageInterpImpl;

interface

uses
  ExtCtrls, Classes, Graphics, Windows;

const
  START_COLOR = clLime;

type
  TPathDirection = (pdNone, pdN, pdNE, pdE, pdSE, pdS, pdSW, pdW, pdNW);
  TPathDirectionSet = set of TPathDirection;


  TFrame = record
    X: integer;
    Y: integer;

    Color: TColor;
    PathDirection: TPathDirection;
  end;
  PFrame = ^TFrame;


  TPointList = class
  private
    FList: TList;
    function GetPoint(const aIndex: integer): PFrame;
    function GetCount: integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure AddPoint(const aX, aY: integer;
      const aColor: TColor; const aDirection: TPathDirection = pdNone);

    property Point[const aIndex: integer]: PFrame read GetPoint; default;
    property Count: integer read GetCount;
  end;

  TBinaryPixelMap = array of array of boolean;
  TColorPixelMap = array of array of TColor;

  TPathPoint = class
  private
    FPathDirection: TPathDirection;
    FX, FY: integer;
    FColor: TColor;
    FPrevious, FNext: TPathPoint;

  public
    constructor Create(const aDirection: TPathDirection; const aX, aY: integer;
      const aColor: TColor; const aPrevious: TPathPoint); reintroduce;

    property PathDirection: TPathDirection read FPathDirection;
    property X: integer read FX;
    property Y: integer read FY;
    property Color: TColor read FColor;
    property Previous: TPathPoint read FPrevious write FPrevious;
    property Next: TPathPoint read FNext write FNext;
  end;

  TPointDirection = class
  private
    FX, FY: integer;
    FNeightBoardToVisit: TPathDirectionSet;
  public
    constructor Create(const aX, aY: integer;
      const aNeightboardToVisit: TPathDirectionSet); reintroduce;

    procedure Visit(const aDirection: TPathDirection);
    property NeightboardToVisit: TPathDirectionSet read FNeightBoardToVisit write FNeightBoardToVisit;
  end;

  TDirectionCache = class
  private
    FList: TStringList;
    FImage: TBinaryPixelMap;
  public
    constructor Create(const aImage: TBinaryPixelMap); reintroduce;
    destructor Destroy; override;

    function GetDirections(const aX, aY: integer): TPointDirection;
  end;

  TImageInterp = class
  private
    FImage: TImage;

    FWidth, FHeight: integer;
    FColors: TColorPixelMap;
    FAllColors: TBinaryPixelMap;
    FBlack: TBinaryPixelMap;
    FRed: TBinaryPixelMap;
    FGreen: TBinaryPixelMap;
    FBlue: TBinaryPixelMap;

    FXAxisA: double;
    FXAxisB: double;

    FYAxisA: double;
    FYAxisB: double;

    FOrigin: TPoint;

    FThinState: boolean;
    FLastRemovedPixels: TBinaryPixelMap;
  public
    constructor Create(const aImage: TImage);

    function ThinAllStep(var aRemovedPixels: TBinaryPixelMap): boolean;
    procedure RemoveStairCaseAll(var aRemovedPixels: TBinaryPixelMap);
    function ThinBlackStep(var aRemovedPixels: TBinaryPixelMap): boolean;
    procedure RemoveStairCaseBlack(var aRemovedPixels: TBinaryPixelMap);
    procedure FindXAxis(var a, b: double);
    procedure FindYAxis(var a, b: double);
    function GetOrigin: TPoint;
    function GetFirstPoint: TPoint;

    property Image: TImage read FImage;
    property Width: integer read FWidth;
    property Height: integer read FHeight;

    property Black: TBinaryPixelMap read FBlack;
    property Red: TBinaryPixelMap read FRed;
    property Green: TBinaryPixelMap read FGreen;
    property Blue: TBinaryPixelMap read FBlue;
    property AllColors: TBinaryPixelMap read FAllColors;
    property Colors: TColorPixelMap read FColors;
    property ThinState: boolean read FThinState;
    property LastRemovedPixels: TBinaryPixelMap read FLastRemovedPixels;

    class procedure SeparateColors(const aImage: TImage; const aWidth, aHeight: integer;
      var aBlack, aRed, aGreen, aBlue, aAllColors: TBinaryPixelMap; var aColors: TColorPixelMap);

    class procedure GetNeightBoardPixels(const aImage: TBinaryPixelMap; const aX, aY: integer;
      var aP1, aP2, aP3, aP4, aP5, aP6, aP7, aP8, aP9: boolean);
    class function ThinStep(var aImage: TBinaryPixelMap; const aWidth, aHeight: integer;
      var aBool: boolean; var aRemovedPixels: TBinaryPixelMap): boolean;
    class procedure Thin(var aImage: TBinaryPixelMap; const aWidth, aHeight: integer; var aRemovedPixels: TBinaryPixelMap);
    class procedure StaircaseRemoval(var aImage: TBinaryPixelMap; const aWidth, aHeight: integer; var aRemovedPixels: TBinaryPixelMap);
    class function IsStaircaseMask(const aImage: TBinaryPixelMap; const aX, aY: integer): boolean;

    class function FindPath(const aImage: TBinaryPixelMap; const aColors: TColorPixelMap; const aX, aY: integer): TPointList;
    class function DirectionDist(const aOrig, aDest: TPathDirection): integer;
    class function GetNearDirection(const aPoint: TPathPoint;
      const aNeightBoardToVisit: TPathDirectionSet): TPathDirection;

    class function GetNeightboardDirections(const aImage: TBinaryPixelMap; const aX, aY: integer): TPathDirectionSet;
    class function OpositeDirection(const aDirection: TPathDirection): TPathDirection;
    class function CountDirections(const aDirections: TPathDirectionSet): integer;
    class function GetFirstDirection(const aDirections: TPathDirectionSet): TPathDirection;

    class procedure FindLine(const aImage: TBinaryPixelMap; aWidth, aHeight: integer;
      var a, b: double);
    class function IntersectLines(const aX, bX, aY, bY: double): TPoint;
    class function GetPoint(const aImage: TBinaryPixelMap; const aX, aY: integer; const aDirection: TPathDirection): boolean;
    class procedure GetDeltaCoord(const aDirection: TPathDirection; out adX, adY: integer);

    class procedure Draw(const aImage: TImage; const aWidth, aHeight: integer;
      const aPixelMap: TBinaryPixelMap; const aColor: integer; const aClear: boolean = True); overload;

    class procedure Draw(const aImage: TImage; const aWidth, aHeight: integer;
      const aPixelMap: TBinaryPixelMap; const aOriginalImage: TImage; const aClear: boolean = True); overload;
  end;


  TImageInterp2 = class
  private
    FImageArray: TBinaryPixelMap;
    FXCoordinatesArray: TBinaryPixelMap;
    FYCoordinatesArray: TBinaryPixelMap;
    FHeight: integer;
    FWidth: integer;
    FRectHeight: integer;
    FRectWidth: integer;
    FIgnoreFactor: double;

    FPointList: TPointList;
    FOrigin: TPoint;
    FXFactor: double;
    FYFactor: double;
  public
    constructor Create(const aImage: TImage);
    destructor Destroy; override;
    procedure GeneratePoints(const aImage: TImage);
    function Thin(const aImage: TImage; aBool: boolean): boolean;
    procedure FindAndSetCoordinates(const aImage: TImage);
    procedure ExportToFile(const aFileName: string);
  published
    property RectHeight: integer read FRectHeight write FRectHeight;
    property RectWidth: integer read FRectWidth write FRectWidth;
    property IgnoreFactor: double read FIgnoreFactor write FIgnoreFactor;

    property XFactor: double read FXFactor;
    property YFactor: double read FYFactor;
  end;

implementation

uses
  Controls,
  SysUtils,
  Math,
  Dialogs;

{ TImageInterp }

constructor TImageInterp2.Create(const aImage: TImage);
var
  x, y: integer;
begin
  FPointList := TPointList.Create;

  FRectHeight := 10;
  FRectWidth := 10;
  FIgnoreFactor := 0;

  FXFactor := 1.0;
  FYFactor := 1.0;

  FWidth := aImage.Width;
  FHeight := aImage.Height;
  SetLength(FImageArray, FWidth, FHeight);
  SetLength(FXCoordinatesArray, FWidth, FHeight);
  SetLength(FYCoordinatesArray, FWidth, FHeight);

  for x := 0 to FWidth - 1 do
    for y := 0 to FHeight - 1 do
    begin
      FImageArray[x , y] := aImage.Canvas.Pixels[x, y] = clBlack;
      FXCoordinatesArray[x , y] := aImage.Canvas.Pixels[x, y] = clBlue;
      FYCoordinatesArray[x , y] := aImage.Canvas.Pixels[x, y] = clRed;
    end;
end;

destructor TImageInterp2.Destroy;
begin
  FPointList.Free;
  inherited;
end;

procedure TImageInterp2.ExportToFile(const aFileName: string);
const
  POINT_LINE = '  LIN {x %g, y %g,z %g,a %g,b %g,c %g} C_VEL';
  MAINSECTION_BEGIN = '; #MainSection#Begin#';
  MAINSECTION_END = '; #MainSection#End#';

var
  vBaseFile: TStringList;
  vFile: TStringList;
  vProgramText: string;
  cPoint, cLine: integer;
  vFormatSettings: TFormatSettings;
begin
  vFormatSettings.DecimalSeparator := '.';

  vFile := TStringList.Create;
  try
    vFile.Add('  $BASE = BASE_DATA[3]');
    vFile.Add('  $TOOL = TOOL_DATA[4]');
    vFile.Add('  $APO.CVEL=100');
    vFile.Add('  $VEL.CP=0.2');

    vFile.Add('');

    vFile.Add('  ; Eixo X');
    vFile.Add(Format(POINT_LINE,
      [0 * FXFactor,
      0 * FYFactor,
      -50.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));
    vFile.Add(Format(POINT_LINE,
      [0 * FXFactor,
      0 * FYFactor,
      0.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));
    vFile.Add(Format(POINT_LINE,
      [80 * FXFactor,
      0 * FYFactor,
      0.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));
    vFile.Add(Format(POINT_LINE,
      [0 * FXFactor,
      0 * FYFactor,
      0.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));

    vFile.Add('');

    vFile.Add('  ; Eixo Y');
    vFile.Add(Format(POINT_LINE,
      [0 * FXFactor,
      80 * FYFactor,
      0.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));
    vFile.Add(Format(POINT_LINE,
      [0 * FXFactor,
      0 * FYFactor,
      0.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));

    vFile.Add('');


    vFile.Add(Format(POINT_LINE,
      [FPointList[0].X * FXFactor,
      FPointList[0].Y * FYFactor,
      -50.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));

    vFile.Add('');

    for cPoint := 0 to FPointList.Count - 1 do
      vFile.Add(Format(POINT_LINE,
        [FPointList[cPoint].X * FXFactor,
        FPointList[cPoint].Y * FYFactor,
        0.0,
        0.0,
        0.0,
        0.0],
        vFormatSettings));

    vFile.Add('');

    vFile.Add(Format(POINT_LINE,
      [FPointList[FPointList.Count - 1].X * FXFactor,
      FPointList[FPointList.Count - 1].Y * FYFactor,
      -50.0,
      0.0,
      0.0,
      0.0],
      vFormatSettings));


    vProgramText := vFile.Text;
  finally
    vFile.Free;
  end;

  vBaseFile := TStringList.Create;
  try
    vBaseFile.LoadFromFile(ExtractFilePath(ParamStr(0))+'Program\base.src');
    vBaseFile.Text := StringReplace(vBaseFile.Text, '#$FileName#', aFileName, [rfReplaceAll]);

    for cLine := 0 to vBaseFile.Count - 1 do
      if vBaseFile[cLine] = MAINSECTION_BEGIN then
      begin
        vBaseFile.Insert(cLine+1, vProgramText);
        break;
      end;

    vBaseFile.SaveToFile(ExtractFilePath(ParamStr(0))+'Program\'+aFileName+'.src');
  finally
    vBaseFile.Free;
  end;

  CopyFile(PChar(ExtractFilePath(ParamStr(0))+'Program\base.dat'),
     PChar(ExtractFilePath(ParamStr(0))+'Program\'+aFileName+'.dat'),
     True);
end;

procedure TImageInterp2.FindAndSetCoordinates(const aImage: TImage);
var
  vTextFile1, vTextFile2: TStringList;

  procedure FindAxis(const aPoints: TBinaryPixelMap; out a, b: double);
  var
    x, y: integer;
    vSumX, vSumY, vSumSquareX, vSumXY: double;
    n: integer;

    vLog: TStringList;
  begin
    if aPoints = FXCoordinatesArray then
      vLog := vTextFile1
    else
      vLog := vTextFile2;

    vSumX := 0;
    vSumY := 0;
    vSumSquareX := 0;
    vSumXY := 0;
    n := 0;
    for x := 0 to FWidth - 1 do
      for y := 0 to FHeight - 1 do
      if aPoints[x, y] then
      begin
        Inc(n);
        vSumX := vSumX + x;
        vSumY := vSumY + y;
        vSumSquareX := vSumSquareX + x*x;
        vSumXY := vSumXY + x*y;
        vLog.Add(IntToStr(x)+';'+IntToStr(y));
      end;

    // Calcula coeficientes para reta y=ax+b
    a := (n*vSumXY-vSumX*vSumY)/(n*vSumSquareX-vSumX*vSumX);
    b := (vSumSquareX*vSumY-vSumXY*vSumX)/(n*vSumSquareX-vSumX*vSumX);
  end;
var
  aX, bX: double;
  aY, bY: double;
begin
  vTextFile1 := TStringList.Create;
  vTextFile2 := TStringList.Create;
  FindAxis(FXCoordinatesArray, aX, bX);
  FindAxis(FYCoordinatesArray, aY, bY);

  vTextFile1.SaveToFile('tst1.txt');
  vTextFile2.SaveToFile('tst2.txt');

  ShowMessage('('+IntToStr(FOrigin.X)+','+IntToStr(FOrigin.Y)+')');

  aImage.Canvas.Pen.Color := clLime;
  aImage.Canvas.Arc(FOrigin.X-5, FOrigin.Y-5,
    FOrigin.X+5, FOrigin.Y+5, 0,0,0,0);
end;

procedure TImageInterp2.GeneratePoints(const aImage: TImage);
begin
(*
var
  cRectX, cRectY: integer;
  x, y: integer;
  vRect: TRect;
  vRectSize: integer;
  vIgnore: boolean;

  vMass: integer;
  vAccX, vAccY: integer;

  vCenterX, vCenterY: integer;

  i: integer;
begin

  aImage.Canvas.Brush.Color := clWhite;
  vRect.Left := 0;
  vRect.Top := 0;
  vRect.Right := FWidth;
  vRect.Bottom := FHeight;
  aImage.Canvas.FillRect(vRect);

  vRectSize := FRectWidth*FRectHeight;
  for cRectX := 0 to (FWidth - 1 div FRectWidth) do
    for cRectY := 0 to (FHeight - 1 div FRectHeight) do
    begin
      vRect.Left := cRectX*FRectWidth;
      vRect.Top := cRectY*FRectHeight;
      vRect.Right := vRect.Left + FRectWidth;
      vRect.Bottom := vRect.Top + FRectHeight;

      vAccX := 0;
      vAccY := 0;
      vMass := 0;

      for x := vRect.Left to vRect.Right do
      begin
        if x >= FWidth then
          break;

        for y := vRect.Top to vRect.Bottom do
        begin
          if y >= FHeight then
            break;

          if FImageArray[x, y] then
          begin
            vAccX := vAccX + x;
            vAccY := vAccY + y;
            vMass := vMass + 1;

            // Redesenha a linha original
            //aImage.Canvas.Pixels[x, y] := clBlack;
          end;
        end;
      end;

      vIgnore := (vMass/vRectSize < FIgnoreFactor);

      if vMass <> 0 then
      begin
        aImage.Canvas.Brush.Color := clBlue;
        //aImage.Canvas.FrameRect(vRect);

        vCenterX := Round(vAccX/vMass);
        vCenterY := Round(vAccY/vMass);

        if not vIgnore then
        begin
          FPointList.AddPoint(vCenterX, vCenterY);
          aImage.Canvas.Pen.Color := clLime;
        end
        else
          aImage.Canvas.Pen.Color := clRed;


        aImage.Canvas.Pixels[vCenterX, vCenterY] := aImage.Canvas.Pen.Color;
          aImage.Canvas.Arc(vCenterX-5, vCenterY-5,
            vCenterX+5, vCenterY+5, 0,0,0,0);
      end;
    end;

  aImage.Canvas.Pen.Color := clBlue;
  for i := 0 to FPointList.Count - 2 do
  begin
    aImage.Canvas.MoveTo(Round(FPointList[i].X), Round(FPointList[i].Y));
    aImage.Canvas.LineTo(Round(FPointList[i+1].X), Round(FPointList[i+1].Y));
  end;
*)
end;

function TImageInterp2.Thin(const aImage: TImage;
  aBool: boolean): boolean;
var
  x, y: integer;
  P2, P3, P4, P5, P6, P7, P8, P9: boolean;

  vThinImage: array of array of boolean;

  vRect: TRect;

  N, B: integer;
begin
  if Assigned(aImage) then
  begin
    aImage.Canvas.Brush.Color := clWhite;
    vRect.Left := 0;
    vRect.Top := 0;
    vRect.Right := FWidth;
    vRect.Bottom := FHeight;
    aImage.Canvas.FillRect(vRect);
  end;


  result := False;
  SetLength(vThinImage, FWidth, FHeight);
  // Por enquanto, não está considerando a borda da figura
  for x := 1 to FWidth - 2 do
    for y := 1 to FHeight - 2 do
      if FImageArray[x, y] then
      begin
        P2 := FImageArray[x, y-1];
        P3 := FImageArray[x+1, y-1];
        P4 := FImageArray[x+1, y];
        P5 := FImageArray[x+1, y+1];
        P6 := FImageArray[x, y+1];
        P7 := FImageArray[x-1, y+1];
        P8 := FImageArray[x-1, y];
        P9 := FImageArray[x-1, y-1];

        // Número de pixels coloridos na vizinhança
        N := Integer(P2)
          +Integer(P3)
          +Integer(P4)
          +Integer(P5)
          +Integer(P6)
          +Integer(P7)
          +Integer(P8)
          +Integer(P9);

        B := Integer(not P2 and P3)
          +Integer(not P3 and P4)
          +Integer(not P4 and P5)
          +Integer(not P5 and P6)
          +Integer(not P6 and P7)
          +Integer(not P7 and P8)
          +Integer(not P8 and P9)
          +Integer(not P9 and P2);

        if not aBool then
        begin
          if (N >= 2) and (N <= 7)
            and (B = 1)
            and not (P2 and P4 and P6)
            and not (P4 and P6 and P8) then
          begin
            result := True;
            if Assigned(aImage) then
              aImage.Canvas.Pixels[x,y] := clWhite;
          end
          else
          begin
            if Assigned(aImage) then
              aImage.Canvas.Pixels[x,y] := clBlack;
            vThinImage[x, y] := True;
          end;
        end
        else
        begin
          if (N >= 2) and (N <= 7)
            and (B = 1)
            and not (P2 and P4 and P8)
            and not (P2 and P6 and P8) then
          begin
            result := True;
            if Assigned(aImage) then
              aImage.Canvas.Pixels[x,y] := clWhite;
          end
          else
          begin
            if Assigned(aImage) then
              aImage.Canvas.Pixels[x,y] := clBlack;
            vThinImage[x, y] := True;
          end;
        end;
      end
      else
      begin
        if Assigned(aImage) then
          aImage.Canvas.Pixels[x,y] := clWhite;
      end;


  for x := 0 to FWidth - 1 do
    for y := 0 to FHeight - 1 do
      FImageArray[x, y] := vThinImage[x, y];
end;

{ TImageInterp }

class function TImageInterp.ThinStep(var aImage: TBinaryPixelMap; const aWidth, aHeight: integer;
  var aBool: boolean; var aRemovedPixels: TBinaryPixelMap): boolean;
var
  x, y: integer;
  P1, P2, P3, P4, P5, P6, P7, P8, P9: boolean;

  NN, B: integer;
begin
  result := False;

  for x := 1 to aWidth - 2 do
    for y := 1 to aHeight - 2 do
      aRemovedPixels[x,y] := False;

  // Por enquanto, não está considerando a borda da figura
  for x := 1 to aWidth - 2 do
    for y := 1 to aHeight - 2 do
      if aImage[x, y] then
      begin
        GetNeightBoardPixels(aImage, x, y, P1, P2, P3, P4, P5, P6, P7, P8, P9);

        // Número de pixels coloridos na vizinhança
        NN := Integer(P2)
          +Integer(P3)
          +Integer(P4)
          +Integer(P5)
          +Integer(P6)
          +Integer(P7)
          +Integer(P8)
          +Integer(P9);

        // Número de transições Branco >> Preto
        B := Integer(not P2 and P3)
          +Integer(not P3 and P4)
          +Integer(not P4 and P5)
          +Integer(not P5 and P6)
          +Integer(not P6 and P7)
          +Integer(not P7 and P8)
          +Integer(not P8 and P9)
          +Integer(not P9 and P2);

        if not aBool then
        begin
          if (NN >= 2) and (NN <= 6)
            and (B = 1)
            and not (P2 and P4 and P6)
            and not (P4 and P6 and P8) then
          begin
            aRemovedPixels[x,y] := True;
            result := True;
          end;
        end
        else
        begin
          if (NN >= 2) and (NN <= 6)
            and (B = 1)
            and not (P2 and P6 and P8)
            and not (P2 and P4 and P8) then
          begin
            aRemovedPixels[x,y] := True;
            result := True;
          end;
        end;
      end;

  for x := 1 to aWidth - 2 do
    for y := 1 to aHeight - 2 do
      aImage[x, y] := aImage[x, y] and not aRemovedPixels[x,y];

  aBool := not aBool;
end;

class function TImageInterp.CountDirections(
  const aDirections: TPathDirectionSet): integer;
var
  cDir: TPathDirection;
begin
  result := 0;
  for cDir := pdN to pdNW do
    if cDir in aDirections then
      Inc(result);
end;

constructor TImageInterp.Create(const aImage: TImage);
begin
  FImage := aImage;
  FWidth := aImage.Picture.Width;
  FHeight := aImage.Picture.Height;

  SetLength(FLastRemovedPixels, FWidth, FHeight);

  SeparateColors(FImage, FWidth, FHeight, FBlack, FRed, FGreen, FBlue, FAllColors, FColors);
end;

class function TImageInterp.DirectionDist(const aOrig,
  aDest: TPathDirection): integer;
var
  vDist: integer;
  vDirCount: integer;
begin
  Assert(aDest <> pdNone);

  vDist := Abs(Ord(aOrig) - Ord(aDest));
  vDirCount := Ord(High(TPathDirection));
  if vDist > (vDirCount div 2) then
    vDist := (vDirCount div 2) - (vDist mod (vDirCount div 2));

  result := vDist;
end;

class procedure TImageInterp.Draw(const aImage: TImage; const aWidth,
  aHeight: integer; const aPixelMap: TBinaryPixelMap;
  const aOriginalImage: TImage; const aClear: boolean);
var
  vRect: TRect;
  x, y: integer;
  vCanvas: TCanvas;
begin
  aImage.Picture.Bitmap.Width := aWidth;
  aImage.Picture.Bitmap.Height := aHeight;
  vCanvas := aImage.Picture.Bitmap.Canvas;
  if aClear then
  begin
    vCanvas.Brush.Color := clWhite;
    vRect.Left := 0;
    vRect.Top := 0;
    vRect.Right := aWidth;
    vRect.Bottom := aHeight;
    vCanvas.FillRect(vRect);
  end;

  for x := 0 to aWidth - 1 do
    for y := 0 to aHeight - 1 do
      if aPixelMap[x, y] then
        vCanvas.Pixels[x, y] := aOriginalImage.Canvas.Pixels[x, y];

end;

class procedure TImageInterp.Draw(const aImage: TImage; const aWidth, aHeight: integer;
  const aPixelMap: TBinaryPixelMap; const aColor: integer; const aClear: boolean);
var
  vRect: TRect;
  x, y: integer;
  vCanvas: TCanvas;
begin
  aImage.Picture.Bitmap.Width := aWidth;
  aImage.Picture.Bitmap.Height := aHeight;
  vCanvas := aImage.Picture.Bitmap.Canvas;
  if aClear then
  begin
    vCanvas.Brush.Color := clWhite;
    vRect.Left := 0;
    vRect.Top := 0;
    vRect.Right := aWidth;
    vRect.Bottom := aHeight;
    vCanvas.FillRect(vRect);
  end;

  for x := 0 to aWidth - 1 do
    for y := 0 to aHeight - 1 do
      if aPixelMap[x, y] then
        vCanvas.Pixels[x, y] := aColor;
end;

class procedure TImageInterp.FindLine(const aImage: TBinaryPixelMap; aWidth, aHeight: integer;
var a, b: double);
var
  x, y: integer;
  vSumX, vSumY, vSumSquareX, vSumSquareY, vSumXY: double;
  vAvgX, vAvgY: double;
//  vSumErrXY: double;
//  vSumSquareErrX, vSumSquareErrY: double;
  n: integer;
  r: double;

  vNum, vDem: double;

//  a1, b1, a2, b2: double;
//  r1, r2: double;
begin
  vSumX := 0;
  vSumY := 0;
  vSumSquareX := 0;
  vSumSquareY := 0;
  vSumXY := 0;
  n := 0;

  for x := 0 to aWidth - 1 do
    for y := 0 to aHeight - 1 do
    if aImage[x, y] then
    begin
      Inc(n);
      vSumX := vSumX + x;
      vSumY := vSumY + y;
      vSumSquareX := vSumSquareX + Sqr(x);
      vSumSquareY := vSumSquareY + Sqr(y);
      vSumXY := vSumXY + x*y;
    end;

  vAvgX := vSumX/n;
  vAvgY := vSumY/n;

  // Calcula coeficientes para reta y=ax+b
  vNum := (n*vSumXY-vSumX*vSumY);
  vDem := (n*vSumSquareX-Sqr(vSumX));
  if (vDem = 0) then
    a := INFINITE
  else
    a := vNum/vDem;

  b := vAvgY-a*vAvgX;
(*
  vSumErrXY := 0;

  for x := 0 to aWidth - 1 do
    for y := 0 to aHeight - 1 do
    if aImage[x, y] then
    begin
      vSumErrXY := vSumErrXY + (x - vAvgX)*(y - vAvgY);
      vSumSquareErrX := vSumSquareErrX+ Sqr(x - vAvgX);
      vSumSquareErrY := vSumSquareErrY+ Sqr(y - vAvgY);
    end;
*)
  vNum := n*vSumXY-vSumX*vSumY;
  vDem := Sqrt( (n*vSumSquareX - Sqr(vSumX))*(n*vSumSquareY - Sqr(vSumY)) );

  if (vDem = 0) then
    r := -1
  else
    r := Abs(vNum/vDem);

  ShowMessage(FloatToStr(r));
end;

class function TImageInterp.FindPath(const aImage: TBinaryPixelMap;
 const aColors: TColorPixelMap; const aX, aY: integer): TPointList;
var
  vDirectionCache: TDirectionCache;

  vFirstPoint: TPathPoint;
  vPoint, vNextPoint: TPathPoint;
  vNearDirection: TPathDirection;

  vNeightboardDirections: TPointDirection;

  dX, dY: integer;
begin
  result := TPointList.Create;
  vDirectionCache := TDirectionCache.Create(aImage);
  try
    vPoint := TPathPoint.Create(pdNone, aX, aY, START_COLOR, nil);

    vNeightboardDirections := vDirectionCache.GetDirections(vPoint.X, vPoint.Y);

    vFirstPoint := vPoint;
    while Assigned(vPoint) do
    begin
      vNearDirection := GetNearDirection(vPoint,
        vNeightboardDirections.NeightboardToVisit);
      if vNearDirection <> pdNone then
      begin
        GetDeltaCoord(vNearDirection, dX, dY);
        vNeightboardDirections.Visit(vNearDirection);


        vNextPoint := TPathPoint.Create(vNearDirection,
          vPoint.X + dX,
          vPoint.Y + dY,
          aColors[vPoint.X + dX, vPoint.Y + dY],
          vPoint);

        vNeightboardDirections := vDirectionCache.GetDirections(vNextPoint.X, vNextPoint.Y);
        vNeightboardDirections.Visit(OpositeDirection(vNearDirection));
      end
      else
        vNextPoint := nil;

      vPoint.Next := vNextPoint;

      // Aproxima pontos por uma reta
      if Assigned(vPoint.Next) and
        Assigned(vPoint.Previous) and
        (vPoint.PathDirection = vNearDirection) and (vPoint.Color = vPoint.Next.Color) then
        begin
          vPoint.Previous.Next := vPoint.Next;
          vPoint.Next.Previous := vPoint.Previous;
        end;

      vPoint := vNextPoint;
    end;

    vPoint := vFirstPoint;
    while Assigned(vPoint) do
    begin
      Result.AddPoint(vPoint.X, vPoint.Y, vPoint.Color, vPoint.PathDirection);
      vPoint := vPoint.Next;
    end;

  finally
    vDirectionCache.Free;
  end;
end;

procedure TImageInterp.FindXAxis(var a, b: double);
begin
  FindLine(FRed, FWidth, FHeight, FXAxisA, FXAxisB);
  a := FXAxisA;
  b := FXAxisB;
end;

procedure TImageInterp.FindYAxis(var a, b: double);
begin
  FindLine(FGreen, FWidth, FHeight, FYAxisA, FYAxisB);
  a := FYAxisA;
  b := FYAxisB;
end;

class function TImageInterp.IsStaircaseMask(const aImage: TBinaryPixelMap;
  const aX, aY: integer): boolean;
var
  P1, P2, P3, P4, P5, P6, P7, P8, P9: boolean;
begin
  GetNeightBoardPixels(aImage, aX, aY, P1, P2, P3, P4, P5, P6, P7, P8, P9);

  result := (P1 and P2 and P8 and not P5 and not P9) or
    (P1 and P2 and P4 and not P3 and not P7) or
    (P1 and P4 and P6 and not P5 and not P9) or
    (P1 and P6 and P8 and not P3 and not P7 );
end;

class function TImageInterp.OpositeDirection(
  const aDirection: TPathDirection): TPathDirection;
begin
  case aDirection of
    pdN:
      result := pdS;
    pdNE:
      result := pdSW;
    pdE:
      result := pdW;
    pdSE:
      result := pdNW;
    pdS:
      result := pdN;
    pdSW:
      result := pdNE;
    pdW:
      result := pdE;
    pdNW:
      result := pdSE;
  else
    result := pdNone;
  end;
end;

class procedure TImageInterp.GetDeltaCoord(const aDirection: TPathDirection;
  out adX, adY: integer);
begin
  case aDirection of
    pdN:
    begin
      adX := 0;
      adY := -1;
    end;
    pdNE:
    begin
      adX := 1;
      adY := -1;
    end;
    pdE:
    begin
      adX := 1;
      adY := 0;
    end;
    pdSE:
    begin
      adX := 1;
      adY := 1;
    end;
    pdS:
    begin
      adX := 0;
      adY := 1;
    end;
    pdSW:
    begin
      adX := -1;
      adY := 1;
    end;
    pdW:
    begin
      adX := -1;
      adY := 0;
    end;
    pdNW:
    begin
      adX := -1;
      adY := -1;
    end;
    else
    begin
      adX := 0;
      adY := 0;
    end;
  end;
end;

class function TImageInterp.GetFirstDirection(
  const aDirections: TPathDirectionSet): TPathDirection;
var
  cDir: TPathDirection;
begin
  result := pdNone;
  for cDir := pdN to pdNW do
    if cDir in aDirections then
    begin
      result := cDir;
      break;
    end;
end;

function TImageInterp.GetFirstPoint: TPoint;
var
  x, y: integer;

  P1, P2, P3, P4, P5, P6, P7, P8, P9: boolean;
  NN: integer;
begin
  for x := 0 to FWidth - 1 do
    for y := 0 to FHeight - 1 do
    begin
      if FAllColors[x,y] and (FColors[x,y] = START_COLOR) then
      begin
        GetNeightBoardPixels(FAllColors, x, y, P1, P2, P3, P4, P5, P6, P7, P8, P9);

        // Número de pixels coloridos na vizinhança
        NN := Integer(P2)
          +Integer(P3)
          +Integer(P4)
          +Integer(P5)
          +Integer(P6)
          +Integer(P7)
          +Integer(P8)
          +Integer(P9);

        if (NN = 1) then
        begin
          result.X := x;
          result.Y := y;
          exit;
        end;

      end;
    end;
end;

class function TImageInterp.GetNearDirection(const aPoint: TPathPoint;
  const aNeightBoardToVisit: TPathDirectionSet): TPathDirection;
var
  vDist, vMinDist: integer;
  cDir: TPathDirection;

  vNearDirections: set of TPathDirection;
  vPreviousNearDirections: set of TPathDirection;
  vPreviousPoint: TPathPoint;
begin
  vMinDist := MaxInt;
  vNearDirections := [];

  for cDir := pdN to pdNW do
    if cDir in aNeightBoardToVisit then
    begin
      vDist := DirectionDist(aPoint.PathDirection, cDir);
      if vDist = vMinDist then
      begin
        vNearDirections := vNearDirections + [cDir];
      end
      else if vDist < vMinDist then
      begin
        vNearDirections := [cDir];
        vMinDist := vDist;
      end;

    end;

  vPreviousPoint := aPoint.Previous;

  if vNearDirections <> [] then
  begin
    while (CountDirections(vNearDirections) <> 1) do
    begin
      if not Assigned(vPreviousPoint) then
        break;

      vMinDist := MaxInt;
      for cDir := pdN to pdNW do
        if cDir in vNearDirections then
        begin
          vDist := DirectionDist(vPreviousPoint.PathDirection, cDir);

          if vDist = vMinDist then
          begin
            vPreviousNearDirections := vNearDirections + [cDir];
          end
          else if vDist < vMinDist then
          begin
            vPreviousNearDirections := [cDir];
            vMinDist := vDist;
          end;
        end;

        vNearDirections := vPreviousNearDirections;
        vPreviousPoint := vPreviousPoint.Previous;
    end;

    result := GetFirstDirection(vNearDirections);
  end
  else
    result := pdNone;
end;

class function TImageInterp.GetNeightboardDirections(
  const aImage: TBinaryPixelMap; const aX, aY: integer): TPathDirectionSet;
var
  cDir: TPathDirection;
begin
  result := [];
  for cDir := pdN to pdNW do
    if GetPoint(aImage, aX, aY, cDir) then
      result := result + [cDir];
end;

class procedure TImageInterp.GetNeightBoardPixels(const aImage: TBinaryPixelMap;
  const aX, aY: integer; var aP1, aP2, aP3, aP4, aP5, aP6, aP7, aP8, aP9: boolean);
begin
  aP1 := GetPoint(aImage, aX, aY, pdNone);
  aP2 := GetPoint(aImage, aX, aY, pdN);
  aP3 := GetPoint(aImage, aX, aY, pdNE);
  aP4 := GetPoint(aImage, aX, aY, pdE);
  aP5 := GetPoint(aImage, aX, aY, pdSE);
  aP6 := GetPoint(aImage, aX, aY, pdS);
  aP7 := GetPoint(aImage, aX, aY, pdSW);
  aP8 := GetPoint(aImage, aX, aY, pdW);
  aP9 := GetPoint(aImage, aX, aY, pdNW);
end;

function TImageInterp.GetOrigin: TPoint;
begin
  FOrigin := IntersectLines(FXAxisA, FXAxisB, FYAxisA, FYAxisB);
  result := FOrigin;
end;

class function TImageInterp.GetPoint(const aImage: TBinaryPixelMap; const aX,
  aY: integer; const aDirection: TPathDirection): boolean;
var
  dX, dY: integer;
  x, y: integer;
begin
  GetDeltaCoord(aDirection, dX, dY);
  x := aX+dX;
  y := aY+dY;

  if (x < 0) or (y < 0) then
    result := False
  else
    result := aImage[x, y];
end;

class function TImageInterp.IntersectLines(const aX, bX, aY,
  bY: double): TPoint;
begin
  // Intersecção das retas
  //  y = aX*x+bX
  //  y = aY*x+bX
  //
  //  aX*x+bX = aY*x+bY
  //  aX*x-aY*x = bY - bX
  //  x(aX-aY) = bY-bX
  //  x = (bY-bX)/(aX-aY)
  //  y = aX*x+bX
  result.X := Round( (bY - bX)/(aX - aY) );
  result.Y := Round( aX * result.X + bX );
end;

procedure TImageInterp.RemoveStairCaseAll(var aRemovedPixels: TBinaryPixelMap);
begin
  StaircaseRemoval(FAllColors, FWidth, FHeight, aRemovedPixels);
  FLastRemovedPixels := aRemovedPixels;
end;

procedure TImageInterp.RemoveStairCaseBlack(
  var aRemovedPixels: TBinaryPixelMap);
begin
  StaircaseRemoval(FBlack, FWidth, FHeight, aRemovedPixels);
end;

class procedure TImageInterp.SeparateColors(const aImage: TImage; const aWidth, aHeight: integer;
  var aBlack, aRed, aGreen, aBlue, aAllColors: TBinaryPixelMap; var aColors: TColorPixelMap);
var
  x, y: integer;
begin
  SetLength(aBlack, aWidth, aHeight);
  SetLength(aRed, aWidth, aHeight);
  SetLength(aGreen, aWidth, aHeight);
  SetLength(aBlue, aWidth, aHeight);
  SetLength(aAllColors, aWidth, aHeight);
  SetLength(aColors, aWidth, aHeight);

  for x := 0 to aWidth - 1 do
    for y := 0 to aHeight - 1 do
    begin

      aColors[x,y] := aImage.Canvas.Pixels[x, y];

      if aImage.Canvas.Pixels[x, y] <> clWhite then
         aAllColors[x, y] := True;

      case aImage.Canvas.Pixels[x, y] of
        clBlack:
          aBlack[x, y] := True;
        clRed:
          aRed[x, y] := True;
        clLime:
          aGreen[x, y] := True;
        clBlue:
          aBlue[x, y] := True;
      end;
    end;
end;

class procedure TImageInterp.StaircaseRemoval(var aImage: TBinaryPixelMap;
  const aWidth, aHeight: integer; var aRemovedPixels: TBinaryPixelMap);
var
  x, y: integer;
  vRemove: boolean;
begin
  for x := 1 to aWidth - 2 do
    for y := 1 to aHeight - 2 do
    begin
      vRemove := IsStaircaseMask(aImage, x, y);
      aRemovedPixels[x,y] := vRemove;
      aImage[x, y] := aImage[x, y] and not vRemove;
    end;
end;

class procedure TImageInterp.Thin(var aImage: TBinaryPixelMap;
  const aWidth, aHeight: integer; var aRemovedPixels: TBinaryPixelMap);
var
  vContinue: boolean;
  vBool: boolean;
begin
  vContinue := True;
  while vContinue do
    vContinue := ThinStep(aImage, aWidth, aHeight, vBool, aRemovedPixels);
end;

function TImageInterp.ThinAllStep(var aRemovedPixels: TBinaryPixelMap): boolean;
begin
  result := ThinStep(FAllColors, FWidth, FHeight, FThinState, aRemovedPixels);
  FLastRemovedPixels := aRemovedPixels;
end;

function TImageInterp.ThinBlackStep(var aRemovedPixels: TBinaryPixelMap): boolean;
begin
  result := ThinStep(FBlack, FWidth, FHeight, FThinState, aRemovedPixels);
  FLastRemovedPixels := aRemovedPixels;
end;

{ TPointList }

procedure TPointList.AddPoint(const aX, aY: integer;
  const aColor: TColor; const aDirection: TPathDirection);
var
  vPoint: PFrame;
begin
  New(vPoint);
  vPoint.X := aX;
  vPoint.Y := aY;
  vPoint.Color := aColor;

  vPoint.PathDirection := aDirection;

  FList.Add(vPoint);
end;

procedure TPointList.AfterConstruction;
begin
  inherited;
  FList := TList.Create;
end;

procedure TPointList.BeforeDestruction;
begin
  inherited;
  FList.Free;
end;

function TPointList.GetCount: integer;
begin
  result := FList.Count;
end;

function TPointList.GetPoint(const aIndex: integer): PFrame;
begin
  result := FList[aIndex];
end;
(*
procedure TPointList.Sort;
var
  i: integer;
  vPoint: PFrame;
  vNextPoint: PFrame;
  vLastPoint: PFrame;
  vMinY: double;
  vDist, vMinDist: double;

  vSortedList: TList;
begin
  vMinY := MaxInt;
  vNextPoint := nil;

  // Procura o primeiro ponto
  for i := 0 to FList.Count - 1 do
  begin
    vPoint := FList[i];
    if vPoint.Y < vMinY then
    begin
      vNextPoint := vPoint;
      vMinY := vNextPoint.Y;
    end;
  end;

  Assert(Assigned(vNextPoint));

  vSortedList := TList.Create;
  try
    vSortedList.Add(vNextPoint);
    FList.Remove(vNextPoint);

    while FList.Count > 0 do
    begin
      vLastPoint := vSortedList[vSortedList.Count-1];
      vMinDist := MaxInt;
      for i  := 0 to FList.Count - 1 do
      begin
        vPoint := FList[i];
        vDist := Sqrt( (vPoint.X-vLastPoint.X)*(vPoint.X-vLastPoint.X) + (vPoint.Y-vLastPoint.Y)*(vPoint.Y-vLastPoint.Y));
        if vDist < vMinDist then
        begin
          vNextPoint := vPoint;
          vMinDist := vDist;
        end;
      end;
      vSortedList.Add(vNextPoint);
      FList.Remove(vNextPoint);
    end;

    FList.Free;
    FList := vSortedList;
  except
    vSortedList.Free;
    raise;
  end;

  FSorted := True;
end;
*)
{ TPathPoint }

constructor TPathPoint.Create(const aDirection: TPathDirection; const aX,
  aY: integer; const aColor: TColor; const aPrevious: TPathPoint);
begin
  FPathDirection := aDirection;
  FX := aX;
  FY := aY;
  FColor := aColor;

  FPrevious := aPrevious;
end;


{ TPointDirection }

constructor TPointDirection.Create(const aX, aY: integer;
  const aNeightboardToVisit: TPathDirectionSet);
begin
  FX := aX;
  FY := aY;
  FNeightBoardToVisit := aNeightboardToVisit;
end;

procedure TPointDirection.Visit(const aDirection: TPathDirection);
begin
  FNeightBoardToVisit := FNeightBoardToVisit - [aDirection];
end;

{ TDirectionCache }

constructor TDirectionCache.Create(const aImage: TBinaryPixelMap);
begin
  FImage := aImage;
  FList := TStringList.Create;
  FList.Sorted := True;
end;

destructor TDirectionCache.Destroy;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    FList.Objects[i].Free;

  FList.Free;
  inherited;
end;

function TDirectionCache.GetDirections(const aX,
  aY: integer): TPointDirection;
var
  vIndex: integer;
begin
  vIndex := FList.IndexOf(IntToStr(aX)+','+IntToStr(aY));
  if vIndex = -1 then
  begin
    result := TPointDirection.Create(aX,
      aY,
      TImageInterp.GetNeightboardDirections(FImage, aX, aY));

    FList.AddObject(IntToStr(aX)+','+IntToStr(aY), result);
  end
  else
    result := FList.Objects[vIndex] as TPointDirection;
end;

end.
