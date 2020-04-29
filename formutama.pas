unit FormUtama;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  starfield, FormTerrain, FormPong;

type

  { TFormMaster }

  TFormMaster = class(TForm)
    btn_Star: TButton;
    btn_Terrain: TButton;
    btn_Pong: TButton;
    procedure btn_PongClick(Sender: TObject);
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

procedure TFormMaster.btn_PongClick(Sender: TObject);
begin
  SpacePong.Show();
end;

procedure TFormMaster.btn_TerrainClick(Sender: TObject);
begin
  FormMatrix.Show();
end;

end.

