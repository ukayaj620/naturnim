unit FormUtama;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls;

type

  { TFormMaster }

  TFormMaster = class(TForm)
    BtnClose: TButton;
    BtnStart: TButton;
    BtnStop: TButton;
    ImageUtama: TImage;
    procedure BtnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    type
      TPlanet = record
        Name: string;
        Pos: TPoint;
        Degree: Double;
        Radius: Double;
        Color: TColor;
        ChildId: array[1..5] of integer;
    end;
    var
      CenterPos: TPoint;
      Planet: array[1..6] of TPlanet;
  public
    procedure Ellipse(point: TPoint; radius: LongInt; pnColorPlt: TColor; degree: Double; hasLine: boolean);
    function CartesiusToMonitor(point: TPoint): TPoint;
  end;

var
  FormMaster: TFormMaster;

implementation

{$R *.lfm}

{ TFormMaster }

procedure TFormMaster.Ellipse(point: TPoint; radius: LongInt; pnColorPlt: TColor; degree: Double; hasLine: boolean);
var
  pointMonitor: TPoint;
begin
  ImageUtama.Canvas.Pen.Color:= pnColorPlt;
  ImageUtama.Canvas.Pen.Width:= 1;
  pointMonitor:= CartesiusToMonitor(point);
  ImageUtama.Canvas.Ellipse(pointMonitor.x-radius,pointMonitor.y-radius, pointMonitor.x+radius, pointMonitor.y+radius);
  if hasLine = true then
  begin
    ImageUtama.Canvas.Pen.Color:= clBlack;
    ImageUtama.Canvas.Pen.Width:= 2;
    ImageUtama.Canvas.Line(pointMonitor.x, pointMonitor.y, pointMonitor.x+round(radius*Cos(degree*PI/180)), pointMonitor.y-round(radius*Sin(degree*PI/180)));
  end;
end;

procedure TFormMaster.FormCreate(Sender: TObject);
begin
  CenterPos.x:= ImageUtama.Width div 2;
  CenterPos.y:= ImageUtama.Height div 2;
  ImageUtama.Canvas.Brush.Color:= clWhite;
  ImageUtama.Canvas.Rectangle(0, 0, ImageUtama.Width, ImageUtama.Height);
end;

procedure TFormMaster.FormShow(Sender: TObject);
begin
  WindowState:= wsFullScreen;
  Planet[1].Name:= 'Mercury';
  Planet[1].Radius:= 25;
  Planet[1].Pos.SetLocation(round(225-Planet[1].Radius), 0);
  Planet[1].Color:= clRed;
  Planet[1].Degree:= 15;
end;

procedure TFormMaster.BtnStartClick(Sender: TObject);
var
  point: TPoint;
begin
  ImageUtama.Canvas.Brush.Color:= clRed;
  point.SetLocation(0,0);
  Ellipse(point, 75, clRed, 30, true);
  ImageUtama.Canvas.Brush.Style:= bsClear;
  Ellipse(point,225,clBlack, 0, false);
  ImageUtama.Canvas.Brush.Color:= Planet[1].Color;
  Ellipse(Planet[1].Pos, round(Planet[1].Radius), Planet[1].Color, Planet[1].Degree, true);
end;

function TFormMaster.CartesiusToMonitor(point: TPoint): TPoint;
var
  tempPoint: TPoint;
begin
  tempPoint.x:= CenterPos.x + point.x;
  tempPoint.y:= CenterPos.y - point.y;
  CartesiusToMonitor:= tempPoint;
end;

end.

