unit ProgUtilsImpl;

interface

type

  IJob = interface
    function GetElapsedTime: integer;
    property ElapsedTime: integer read GetElapsedTime;
  end;

  TJob = class(TInterfacedObject,IJob)
  private
    FStartTime: TDatetime;
    function GetElapsedTime: integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ElapsedTime: integer read GetElapsedTime;
  end;

  function StartJob: IJob;

implementation

uses
  Forms,
  Controls,
  SysUtils,
  DateUtils;

function StartJob: IJob;
begin
  result := TJob.Create;
end;

{ TJob }

procedure TJob.AfterConstruction;
begin
  inherited;
  Screen.Cursor := crHourGlass;
  FStartTime := Now;
end;

procedure TJob.BeforeDestruction;
begin
  inherited;
  Screen.Cursor := crDefault;
end;

function TJob.GetElapsedTime: integer;
var
  vTimeCount: TDateTime;
begin
  vTimeCount := Now - FStartTime;
  result := MilliSecondOf(vTimeCount);
end;

end.
