program solar_system;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormSolar, starfield, FormUtama
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMaster, FormMaster);
  Application.CreateForm(TFormSol, FormSol);
  Application.CreateForm(TFormStar, FormStar);
  Application.Run;
end.

