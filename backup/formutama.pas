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
    BtnRotate: TButton;
    ImageUtama: TImage;
    FPS: TTimer;
    procedure BtnRotateClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FPSStopTimer(Sender: TObject);
    procedure FPSTimer(Sender: TObject);

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
    // core function
    function CartesiusToMonitor(point: TPoint): TPoint;

    // drawing utility
    procedure clearCanvas();
    procedure noStroke();
    procedure stroke(c: TColor);
    procedure noFill();
    procedure fill(c: TColor);
    procedure strokeWeight(weight: double);
    procedure ellipse(x: double; y: double ; radius: double);
    procedure ellipse(pos: TPoint; radius: double);
    procedure rect(x: double; y: double; w: double; h: double);

    // main functions
    procedure Revolution(index: integer; revolutionDegree: double);
  end;

var
  FormMaster: TFormMaster;

implementation

{$R *.lfm}

{ TFormMaster }


// core function
function TFormMaster.CartesiusToMonitor(point: TPoint): TPoint;
var
  tempPoint: TPoint;
begin
  tempPoint.x:= CenterPos.x + point.x;
  tempPoint.y:= CenterPos.y - point.y;
  CartesiusToMonitor:= tempPoint;
end;


// drawing utility
procedure TFormMaster.clearCanvas();
begin
  noStroke();
  fill(clWhite);
  rect(0, 0, ImageUtama.Canvas.Width, ImageUtama.Canvas.Height);
end;

procedure TFormMaster.noStroke();
begin
  ImageUtama.Canvas.Pen.Style := psClear;
end;

procedure TFormMaster.stroke(c: TColor);
begin
  ImageUtama.Canvas.Pen.Style := psSolid;
  ImageUtama.Canvas.Pen.Color := c;
end;

procedure TFormMaster.noFill();
begin
  ImageUtama.Canvas.Brush.Style := bsclear;
end;

procedure TFormMaster.fill(c: TColor);
begin
  ImageUtama.Canvas.Brush.Style := bssolid;
  ImageUtama.Canvas.Brush.Color := c;
end;

procedure TFormMaster.strokeWeight(weight: double);
begin
  ImageUtama.Canvas.Pen.Width := round(weight);
end;

procedure TFormMaster.Ellipse(x: double; y: double; radius: double);
var
  pointKartesius: TPoint;
  pointMonitor: TPoint;
begin
  pointkartesius.x := round(x);
  pointkartesius.y := round(y);

  pointMonitor:= CartesiusToMonitor(pointkartesius);

  ImageUtama.Canvas.Ellipse(
    pointMonitor.x - round(radius),
    pointMonitor.y - round(radius),
    pointMonitor.x + round(radius),
    pointMonitor.y + round(radius)
  );
end;

procedure TFormMaster.Ellipse(pos: TPoint; radius: double);
var
  pointMonitor: TPoint;
begin
  pointMonitor:= CartesiusToMonitor(pos);

  ImageUtama.Canvas.Ellipse(
    pointMonitor.x - round(radius),
    pointMonitor.y - round(radius),
    pointMonitor.x + round(radius),
    pointMonitor.y + round(radius)
  );
end;

procedure TFormMaster.rect(x: double; y: double; w: double; h: double);
begin
  ImageUtama.Canvas.Rectangle(round(x), round(y), round(x+w), round(y+h));
end;


// Main Function

procedure TFormMaster.Revolution(index: integer; revolutionDegree: double);
var
  tempPoint: TPoint;
  rad: double;
begin
  tempPoint.setLocation(Planet[index].Pos.x, Planet[index].Pos.y);
  rad:= revolutionDegree*PI/180;

  Planet[index].Pos.SetLocation(
    round(tempPoint.x*Cos(rad)-tempPoint.y*Sin(rad)),
    round(tempPoint.x*Sin(rad)+tempPoint.y*Cos(rad))
  );
end;


// Event Handled Function

procedure TFormMaster.FormCreate(Sender: TObject);
begin
  FPS.Enabled:= false;

  CenterPos.x:= ImageUtama.Width div 2;
  CenterPos.y:= ImageUtama.Height div 2;

  clearCanvas(); // First time single Clear = Black Screen (???)
  clearCanvas();
end;

procedure TFormMaster.FormShow(Sender: TObject);
begin
  Planet[1].Name:= 'Mercury';
  Planet[1].Radius:= 25;
  Planet[1].Pos.SetLocation(225, 0);
  Planet[1].Color:= clRed;
  Planet[1].Degree:= 20;

  Planet[2].Name:= 'Venus';
  Planet[2].Radius:= 50;
  Planet[2].Pos.SetLocation(300,0);
  Planet[2].Color:= clYellow;
  Planet[2].Degree:= 17;

  Planet[3].Name:= 'Earth';
  Planet[3].Radius:= 55;
  Planet[3].Pos.SetLocation(400, 0);
  Planet[3].Color:= clBlue;
  Planet[3].Degree:= 15;

  Planet[4].Name:= 'Mars';
  Planet[4].Radius:= 35;
  Planet[4].Pos.SetLocation(600, 0);
  Planet[4].Color:= clRed;
  Planet[4].Degree:= 12;

  Planet[5].Name:= 'Moon';
  Planet[5].Radius:= 15;
  Planet[5].Pos.SetLocation(90, 0);
  Planet[5].Color:= clGray;
  Planet[5].Degree:= 12;

  Planet[6].Name:= 'Deimos';
  Planet[6].Radius:= 20;
  Planet[6].Pos.SetLocation(100, 0);
  Planet[6].Color:= clGreen;
  Planet[6].Degree:= 12;
end;

procedure TFormMaster.FPSTimer(Sender: TObject);
begin
  clearCanvas();

  stroke(clBlack);

  fill(clRed);
  Ellipse(0, 0, 125);

  noFill();
  Ellipse(0, 0, 225);
  Ellipse(0, 0, 300);
  Ellipse(0, 0, 400);
  Ellipse(0, 0, 600);
  BtnRotateClick(Sender);
end;

procedure TFormMaster.BtnStartClick(Sender: TObject);
begin
  FPS.Enabled:= true;
end;

procedure TFormMaster.BtnRotateClick(Sender: TObject);
var
  i: integer;
begin
  Revolution(1, 30); // Revolusi
  Revolution(2, 20);
  Revolution(3, 15);
  Revolution(4, 12);
  Revolution(5, 45);
  Revolution(6, 30);


  // Setup Canvas
  stroke(clBlack);
  strokeWeight(1);

  // Draw Planet
  for i:= 1 to 4 do
  begin
    fill(Planet[i].Color);
    Ellipse(Planet[i].Pos, Planet[i].Radius);
  end;

  // Draw Moon
  fill(Planet[5].Color);
  Ellipse(Planet[5].Pos + Planet[3].Pos, Planet[5].Radius);

  fill(Planet[6].Color);
  Ellipse(Planet[6].Pos + Planet[4].Pos, Planet[6].Radius);
end;

end.

