unit FormUtama;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FormSolar, starfield;

type

  { TFormMaster }

  TFormMaster = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  FormMaster: TFormMaster;

implementation

{$R *.lfm}

{ TFormMaster }

procedure TFormMaster.Button1Click(Sender: TObject);
begin
  FormSol.show();
end;

procedure TFormMaster.Button2Click(Sender: TObject);
begin
  FormStar.Show();
end;

end.

