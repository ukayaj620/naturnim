unit starfield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
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
  Form1: TForm1;

implementation

// The 3D Library
{%region /fold}

/// core function
{%region /fold}
function TForm1.projectTo2D(x:double; y:double; z:double): TPoint;
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

function TForm1.projectTo2D(pos: Vector3): TPoint;
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
procedure TForm1.clearCanvas();
begin
  noStroke();
  fill(clWhite);
  Image1.Canvas.rectangle(0, 0, Image1.Canvas.Width, Image1.Canvas.Height);
end;

procedure TForm1.noStroke();
begin
  Image1.Canvas.Pen.Style := psClear;
end;

procedure TForm1.stroke(c: TColor);
begin
  Image1.Canvas.Pen.Style := psSolid;
  Image1.Canvas.Pen.Color := c;
end;

procedure TForm1.noFill();
begin
  Image1.Canvas.Brush.Style := bsclear;
end;

procedure TForm1.fill(c: TColor);
begin
  Image1.Canvas.Brush.Style := bssolid;
  Image1.Canvas.Brush.Color := c;
end;

procedure TForm1.strokeWeight(weight: double);
begin
  Image1.Canvas.Pen.Width := round(weight);
end;

procedure TForm1.Ellipse(x: double; y: double; z: double; radius: double);
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

procedure TForm1.Ellipse(pos: Vector3; radius: double);
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

procedure TFOrm1.line(x1: double; y1: double; z1: double; x2: double; y2: double; z2: double);
var
  pointMonitor1: TPoint;
  pointMonitor2: TPoint;
begin
  pointMonitor1 := projectTo2D(x1,y1,z1);
  pointMonitor2 := projectTo2D(x2,y2,z2);

  Image1.Canvas.Line(pointMonitor1.x, pointMonitor1.y, pointMonitor2.x, pointMonitor2.y);
end;

procedure TFOrm1.line(pos1: Vector3; pos2: Vector3);
var
  pointMonitor1: TPoint;
  pointMonitor2: TPoint;
begin
  pointMonitor1 := projectTo2D(pos1.x, pos1.y, pos1.z);
  pointMonitor2 := projectTo2D(pos2.x, pos2.y, pos2.z);

  Image1.Canvas.Line(pointMonitor1.x, pointMonitor1.y, pointMonitor2.x, pointMonitor2.y);
end;

procedure TFOrm1.beginShape(size: LongInt);
begin
  setLength(list_vertex, size);
  list_vertex_count := 0;
end;

procedure TFOrm1.vertex(x: double; y:double; z:double);
var
  temp: Vector3;
begin
  temp.x := x;
  temp.y := y;
  temp.z := z;

  list_vertex_count := list_vertex_count + 1;
  list_vertex[list_vertex_count] := temp;
end;

procedure TFOrm1.vertex(pos: Vector3);
begin
  list_vertex_count := list_vertex_count + 1;
  list_vertex[list_vertex_count] := pos;
end;

procedure TFOrm1.endShape();
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
procedure TForm1.FormCreate(Sender: TObject);
var
  titik: Array[1..8] of Vector3;
  k : integer;
begin
  // Setup
  CONST_M := 200;

  // draw
  clearCanvas();
  clearCanvas();

  stroke(clBlack);
  fill(clRed);

  titik[1].x :=   50;
  titik[1].y :=  100;
  titik[1].z :=  100;

  titik[2].x :=   50;
  titik[2].y := -100;
  titik[2].z :=  100;

  titik[3].x := -150;
  titik[3].y := -100;
  titik[3].z :=  100;

  titik[4].x := -150;
  titik[4].y :=  100;
  titik[4].z :=  100;

  titik[5].x :=   50;
  titik[5].y :=  100;
  titik[5].z := -100;

  titik[6].x :=   50;
  titik[6].y := -100;
  titik[6].z := -100;

  titik[7].x := -150;
  titik[7].y := -100;
  titik[7].z := -100;

  titik[8].x := -150;
  titik[8].y :=  100;
  titik[8].z := -100;

  beginShape(8);

  vertex(titik[1]);
  vertex(titik[2]);
  vertex(titik[4]);
  vertex(titik[3]);
  vertex(titik[8]);
  vertex(titik[7]);
  vertex(titik[5]);
  vertex(titik[6]);
  vertex(titik[1]);
  vertex(titik[2]);

  endShape();

  // titik
  for k:=1 to 8 do
  begin
    ellipse(titik[k], 10);
  end;

end;


{$R *.lfm}

end.

