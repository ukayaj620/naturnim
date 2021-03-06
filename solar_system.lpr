program solar_system;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormUtama, starfield, FormTerrain, FormPong
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMaster, FormMaster);
  Application.CreateForm(TFormStar, FormStar);
  Application.CreateForm(TFormMatrix, FormMatrix);
  Application.CreateForm(TSpacePong, SpacePong);
  Application.Run;
end.

