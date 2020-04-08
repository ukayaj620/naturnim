unit starfield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls;

type

  { TFormStar }

  TFormStar = class(TForm)
    ed_kecepatan: TEdit;
    Image1: TImage;
    lbl_kecepatan: TLabel;
    ed_radius: TEdit;
    lbl_radius: TLabel;
    tb_radius: TTrackBar;
    Timer1: TTimer;
    tb_kecepatan: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure tb_kecepatanChange(Sender: TObject);
    procedure tb_radiusChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    type
      Vector3 = record
        x:double;
        y:double;
        z:double;
    end;

    var
      CONST_M: double;
      list_vertex: array of Vector3;
      list_vertex_count: Longint;

      titik: Array[1..500] of Vector3;
      kecepatan: double;
  public
    // The 3D Library
    {%region /fold}

    // core function
    function projectTo2D(x:double; y:double; z:double): TPoint;
    function projectTo2D(pos:Vector3): TPoint;

    // drawing utility
    procedure clearCanvas();
    procedure noStroke();
    procedure stroke(c: TColor);
    procedure noFill();
    procedure fill(c: TColor);
    procedure strokeWeight(weight: double);
    procedure ellipse(x: double; y: double; z: double; radius: double);
    procedure ellipse(pos: Vector3; radius: double);
    procedure line(x1: double; y1: double; z1: double; x2: double; y2: double; z2: double);
    procedure line(pos1: Vector3; pos2: Vector3);
    procedure beginShape(size: LongInt);
    procedure vertex(x: double; y:double; z:double);
    procedure vertex(pos: Vector3);
    procedure endShape();
    {%endregion}
  end;

var
  FormStar: TFormStar;

implementation

// The 3D Library
{%region /fold}

/// core function
{%region /fold}
function TFormStar.projectTo2D(x:double; y:double; z:double): TPoint;
var
  hasil: TPoint;
begin
  if (z <> CONST_M) and (CONST_M <> 0) then
  begin
    hasil.x := Round(Image1.Canvas.Width/2  + x / (1-z/CONST_M));
    hasil.y := Round(Image1.Canvas.Height/2 - y / (1-z/CONST_M));
  end;
  projectTo2D := hasil;
end;

function TFormStar.projectTo2D(pos: Vector3): TPoint;
var
  hasil: TPoint;
begin
  if (pos.z <> CONST_M) and (CONST_M <> 0) then
  begin
    hasil.x := Round(Image1.Canvas.Width/2  + pos.x / (1-pos.z/CONST_M));
    hasil.y := Round(Image1.Canvas.Height/2 - pos.y / (1-pos.z/CONST_M));
  end;
  projectTo2D := hasil;
end;
{%endregion}

/// drawing utility
{%region /fold}
procedure TFormStar.clearCanvas();
begin
  noStroke();
  fill(clBlack);
  Image1.Canvas.rectangle(0, 0, Image1.Canvas.Width, Image1.Canvas.Height);
end;

procedure TFormStar.noStroke();
begin
  Image1.Canvas.Pen.Style := psClear;
end;

procedure TFormStar.stroke(c: TColor);
begin
  Image1.Canvas.Pen.Style := psSolid;
  Image1.Canvas.Pen.Color := c;
end;

procedure TFormStar.noFill();
begin
  Image1.Canvas.Brush.Style := bsclear;
end;

procedure TFormStar.fill(c: TColor);
begin
  Image1.Canvas.Brush.Style := bssolid;
  Image1.Canvas.Brush.Color := c;
end;

procedure TFormStar.strokeWeight(weight: double);
begin
  Image1.Canvas.Pen.Width := round(weight);
end;

procedure TFormStar.Ellipse(x: double; y: double; z: double; radius: double);
var
  pointMonitor: TPoint;
begin
  pointMonitor:= projectTo2D(x,y,z);

  if z = CONST_M then
  begin
    radius := 0;
  end
  else
  begin
    radius := radius / (1-z/CONST_M);
  end;

  Image1.Canvas.Ellipse(
    pointMonitor.x - round(radius),
    pointMonitor.y - round(radius),
    pointMonitor.x + round(radius),
    pointMonitor.y + round(radius)
  );
end;

procedure TFormStar.Ellipse(pos: Vector3; radius: double);
var
  pointMonitor: TPoint;
begin
  pointMonitor:= projectTo2D(pos.x,pos.y,pos.z);

  if pos.z = CONST_M then
  begin
    radius := 0;
  end
  else
  begin
    radius := radius / (1-pos.z/CONST_M);
  end;

  Image1.Canvas.Ellipse(
    pointMonitor.x - round(radius),
    pointMonitor.y - round(radius),
    pointMonitor.x + round(radius),
    pointMonitor.y + round(radius)
  );
end;

procedure TFormStar.line(x1: double; y1: double; z1: double; x2: double; y2: double; z2: double);
var
  pointMonitor1: TPoint;
  pointMonitor2: TPoint;
begin
  pointMonitor1 := projectTo2D(x1,y1,z1);
  pointMonitor2 := projectTo2D(x2,y2,z2);

  if (z1 <> CONST_M) and (z2 <> CONST_M) and (CONST_M <> 0) then
  begin
    Image1.Canvas.Line(pointMonitor1.x, pointMonitor1.y, pointMonitor2.x, pointMonitor2.y);
  end;
end;

procedure TFormStar.line(pos1: Vector3; pos2: Vector3);
var
  pointMonitor1: TPoint;
  pointMonitor2: TPoint;
begin
  pointMonitor1 := projectTo2D(pos1.x, pos1.y, pos1.z);
  pointMonitor2 := projectTo2D(pos2.x, pos2.y, pos2.z);


  if (pos1.z <> CONST_M) and (pos2.z <> CONST_M) and (CONST_M <> 0) then
  begin
    Image1.Canvas.Line(pointMonitor1.x, pointMonitor1.y, pointMonitor2.x, pointMonitor2.y);
  end;
end;

procedure TFormStar.beginShape(size: LongInt);
begin
  setLength(list_vertex, size);
  list_vertex_count := 0;
end;

procedure TFormStar.vertex(x: double; y:double; z:double);
var
  temp: Vector3;
begin
  temp.x := x;
  temp.y := y;
  temp.z := z;

  list_vertex_count := list_vertex_count + 1;
  list_vertex[list_vertex_count] := temp;
end;

procedure TFormStar.vertex(pos: Vector3);
begin
  list_vertex_count := list_vertex_count + 1;
  list_vertex[list_vertex_count] := pos;
end;

procedure TFormStar.endShape();
var
  k : longInt;
begin
  if list_vertex_count > 1 then
  begin
     line(list_vertex[1], list_vertex[2]);
  end;

  if list_vertex_count > 2 then
  begin
    for k:=3 to list_vertex_count do
    begin
      line(list_vertex[k], list_vertex[k-1]);
      line(list_vertex[k], list_vertex[k-2]);
    end;
  end;
end;
{%endregion}
{%endregion}


// EVEBT HANDLING

// Setup
procedure TFormStar.FormCreate(Sender: TObject);
var
  k : integer;
begin
  CONST_M := 200;

  Randomize();
  for k:=1 to 500 do
  begin
    titik[k].x := -2000 + Random() * 4000;
    titik[k].y := -2000 + Random() * 4000;
    titik[k].z := -2000 + Random() * 2000;
  end;

  kecepatan := 0;
end;

procedure TFormStar.tb_kecepatanChange(Sender: TObject);
begin
  ed_kecepatan.text := inttostr(tb_kecepatan.Position);
end;

procedure TFormStar.tb_radiusChange(Sender: TObject);
begin
  ed_radius.text := inttostr(tb_radius.Position);
end;

// draw
procedure TFormStar.Timer1Timer(Sender: TObject);
var
  k : integer;
begin

  // update
  kecepatan := tb_Kecepatan.Position;

  for k:=1 to 500 do
  begin
    titik[k].z := titik[k].z + kecepatan;

    if titik[k].z > CONST_M then
    begin
       titik[k].x := -2000 + Random() * 4000;
       titik[k].y := -2000 + Random() * 4000;
       titik[k].z := titik[k].z -2000;
    end;
  end;


  // draw
  clearCanvas();

  stroke(clWhite);
  fill(clWhite);
  for k:=1 to 500 do
  begin
    line(titik[k].x, titik[k].y, titik[k].z, titik[k].x, titik[k].y, titik[k].z - 4*kecepatan);
    ellipse(titik[k], tb_radius.Position);
  end;
end;


{$R *.lfm}

end.

