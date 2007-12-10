program TrajGen;

uses
  Forms,
  MainFormImpl in 'MainFormImpl.pas' {MainForm},
  ImageInterpImpl in 'ImageInterpImpl.pas',
  ProgUtilsImpl in 'ProgUtilsImpl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
