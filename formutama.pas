unit FormUtama;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  starfield, FormTerrain;

type

  { TFormMaster }

  TFormMaster = class(TForm)
    btn_Star: TButton;
    btn_Terrain: TButton;
    procedure btn_StarClick(Sender: TObject);
    procedure btn_TerrainClick(Sender: TObject);
  private

  public

  end;

var
  FormMaster: TFormMaster;

implementation

{$R *.lfm}

{ TFormMaster }

procedure TFormMaster.btn_StarClick(Sender: TObject);
begin
  FormStar.Show();
end;

procedure TFormMaster.btn_TerrainClick(Sender: TObject);
begin
  FormMatrix.Show();
end;

end.

