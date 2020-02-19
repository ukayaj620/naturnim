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
  private
    CenterPos: TPoint;
  public
    procedure Ellipse(point: TPoint; radius: LongInt; pnColor: TColor);
    function CartesiusToMonitor(point: TPoint): TPoint;
  end;

var
  FormMaster: TFormMaster;

implementation

{$R *.lfm}

{ TFormMaster }

procedure TFormMaster.Ellipse(point: TPoint; radius: LongInt; pnColor: TColor);
var
  pointMonitor: TPoint;
begin
  ImageUtama.Canvas.Pen.Color:= pnColor;
  ImageUtama.Canvas.Pen.Width:= 1;
  pointMonitor:= CartesiusToMonitor(point);
  ImageUtama.Canvas.Ellipse(pointMonitor.x-radius,pointMonitor.y-radius, pointMonitor.x+radius, pointMonitor.y+radius);
end;

procedure TFormMaster.FormCreate(Sender: TObject);
begin
  CenterPos.x:= round(ImageUtama.Width);
  CenterPos.y:= round(ImageUtama.Height);
  ImageUtama.Canvas.Brush.Color:= clWhite;
  ImageUtama.Canvas.Rectangle(0, 0, ImageUtama.Width, ImageUtama.Height);
end;

procedure TFormMaster.BtnStartClick(Sender: TObject);
var
  point: TPoint;
begin
  ImageUtama.Canvas.Brush.Color:= clRed;
  point.SetLocation(0,0);
  Ellipse(point, 100, clRed);
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

