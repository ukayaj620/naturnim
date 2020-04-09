unit FormTerrain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Math;

type

  { TFormMatrix }

  TFormMatrix = class(TForm)
    EditDx: TEdit;
    EditDz: TEdit;
    EditVt: TEdit;
    EditS: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Timer1: TTimer;
    TrackBarDx: TTrackBar;
    TrackBarDz: TTrackBar;
    TrackBarVt: TTrackBar;
    TrackBarSize: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBarVtChange(Sender: TObject);
    procedure TrackBarDxChange(Sender: TObject);
    procedure TrackBarDzChange(Sender: TObject);
    procedure TrackBarSizeChange(Sender: TObject);
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
      w, h: integer;
      dx, dy, dt, vt: double;
      isShow: boolean;
      sizeBox: integer;
    const
      p : array[0..255] of byte = (
        151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
        140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148,
        247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32,
        57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175,
        74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122,
        60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54,
        65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169,
        200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64,
        52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212,
        207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213,
        119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
        129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104,
        218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241,
        81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157,
        184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93,
        222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180);

  public
     // The 3D Library
    {%region /fold}

    // core function
    function projectTo2D(x:double; y:double; z:double): TPoint;
    function projectTo2D(pos:Vector3): TPoint;
    function rotateX(x: double; y: double; z: double; degree: double): Vector3;
    function rotateX(pos: Vector3; degree: double): Vector3;

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

    function lerp(fade: double; d1: double; d2: double): double;
    function fade(t: double): double;
    function grad(hash: integer; x: double; y: double): double;
    function noisePerlin(x: double; y: double): double;
    function customPerlinNoise(x: double; y: double): double;
    {%endregion}

  end;

var
  FormMatrix: TFormMatrix;

implementation

{$R *.lfm}

// Perlin Noise Library
{%region /fold}
function TFormMatrix.lerp(fade: double; d1: double; d2: double): double;
begin
  lerp:= ((1.0 - fade) * d1 + fade * d2);
end;

function TFormMatrix.fade(t: double): double;
begin
  fade:= t * t * t * (t * (t * 6 - 15) + 10);
end;

function TFormMatrix.grad(hash: integer; x: double; y: double): double;
begin
  case (hash AND 3) of
    0:
      grad:= x + y;
    1:
      grad:= -x + y;
    2:
      grad:= x - y;
    3:
      grad:= -x - y;
  end;
end;

function TFormMatrix.noisePerlin(x: double; y: double): double;
var
  xi, yi, g1, g2, g3, g4: integer;
  xf, yf, d1, d2, d3, d4: double;
  xff, yff, x1Inter, x2Inter, yInter: double;
begin
  xi:= floor(x) AND 255;
  yi:= floor(y) AND 255;
  g1:= p[p[xi] + yi];
  g2:= p[p[xi + 1] + yi];
  g3:= p[p[xi] + yi + 1];
  g4:= p[p[xi + 1] + yi + 1];

  xf:= x - floor(x);
  yf:= y - floor(y);

  d1:= grad(g1, xf, yf);
  d2:= grad(g2, xf - 1, yf);
  d3:= grad(g3, xf, yf - 1);
  d4:= grad(g4, xf - 1, yf - 1);

  xff:= fade(xf);
  yff:= fade(yf);

  x1Inter:= lerp(xff, d1, d2);
  x2Inter:= lerp(xff, d3, d4);
  yInter:= lerp(yff, x1Inter, x2Inter);

  yInter := yInter + 0.5;

  if yInter < 0 then
     yInter := 0;
  if yInter > 1 then
     yInter := 1;

  noisePerlin:= yInter;
end;

function TFormMatrix.customPerlinNoise(x: double; y: double): double;
begin
  x := (x+w) * dx/(w*2);
  y := (y+w) * dy/(w*2);
  customPerlinNoise := noisePerlin(x, y-dt);
end;
{%endregion}

// The 3D Library
{%region /fold}

/// core function
{%region /fold}
function TFormMatrix.projectTo2D(x:double; y:double; z:double): TPoint;
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

function TFormMatrix.projectTo2D(pos: Vector3): TPoint;
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

function TFormMatrix.rotateX(x: double; y: double; z: double; degree: double): Vector3;
var
  hasil: Vector3;
  rad: double;
begin
  rad:= degree*PI/180;
  hasil.x:= x;
  hasil.y:= y*Cos(rad) - z*Sin(rad);
  hasil.z:= y*Sin(rad) + z*Cos(rad);
  rotateX:= hasil;
end;

function TFormMatrix.rotateX(pos: Vector3; degree: double): Vector3;
var
  hasil: Vector3;
  rad: double;
begin
  rad:= degree*PI/180;
  hasil.x:= pos.x;
  hasil.y:= pos.y*Cos(rad) - pos.z*Sin(rad);
  hasil.z:= pos.y*Sin(rad) + pos.z*Cos(rad);
  rotateX:= hasil;
end;

{%endregion}

/// drawing utility
{%region /fold}
procedure TFormMatrix.clearCanvas();
begin
  noStroke();
  fill(clBlack);
  Image1.Canvas.rectangle(0, 0, Image1.Canvas.Width, Image1.Canvas.Height);
end;

procedure TFormMatrix.noStroke();
begin
  Image1.Canvas.Pen.Style := psClear;
end;

procedure TFormMatrix.stroke(c: TColor);
begin
  Image1.Canvas.Pen.Style := psSolid;
  Image1.Canvas.Pen.Color := c;
end;

procedure TFormMatrix.noFill();
begin
  Image1.Canvas.Brush.Style := bsclear;
end;

procedure TFormMatrix.fill(c: TColor);
begin
  Image1.Canvas.Brush.Style := bssolid;
  Image1.Canvas.Brush.Color := c;
end;

procedure TFormMatrix.strokeWeight(weight: double);
begin
  Image1.Canvas.Pen.Width := round(weight);
end;

procedure TFormMatrix.Ellipse(x: double; y: double; z: double; radius: double);
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

procedure TFormMatrix.Ellipse(pos: Vector3; radius: double);
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

procedure TFormMatrix.line(x1: double; y1: double; z1: double; x2: double; y2: double; z2: double);
var
  pointMonitor1: TPoint;
  pointMonitor2: TPoint;
begin
  pointMonitor1 := projectTo2D(x1,y1,z1);
  pointMonitor2 := projectTo2D(x2,y2,z2);

  if (z1 < CONST_M) and (z2 < CONST_M) and (CONST_M <> 0) then
  begin
    Image1.Canvas.Line(pointMonitor1.x, pointMonitor1.y, pointMonitor2.x, pointMonitor2.y);
  end;
end;

procedure TFormMatrix.line(pos1: Vector3; pos2: Vector3);
var
  pointMonitor1: TPoint;
  pointMonitor2: TPoint;
begin
  pointMonitor1 := projectTo2D(pos1.x, pos1.y, pos1.z);
  pointMonitor2 := projectTo2D(pos2.x, pos2.y, pos2.z);

  if (pos1.z < CONST_M) and (pos2.z < CONST_M) and (CONST_M <> 0) then
  begin
    Image1.Canvas.Line(pointMonitor1.x, pointMonitor1.y, pointMonitor2.x, pointMonitor2.y);
  end;
end;

procedure TFormMatrix.beginShape(size: LongInt);
begin
  setLength(list_vertex, size);
  list_vertex_count := 0;
end;

procedure TFormMatrix.vertex(x: double; y:double; z:double);
var
  temp: Vector3;
  triangle: array[1..3] of TPoint;
begin
  temp.x := x;
  temp.y := y;
  temp.z := z;

  list_vertex[list_vertex_count] := temp;

  // Metode pembuatan segitiga yg digunakan adalah TRIANGLE_STRIP

  if list_vertex_count > 0 then
  begin
     line(list_vertex[0], list_vertex[1]);
  end;

  if list_vertex_count > 1 then
  begin
    triangle[1] := ProjectTo2D(list_vertex[list_vertex_count-2]);
    triangle[2] := ProjectTo2D(list_vertex[list_vertex_count-1]);
    triangle[3] := ProjectTo2D(list_vertex[list_vertex_count]);
    Image1.Canvas.Polygon(triangle,true);  // parameter (array of Tpoint, culling:Boolean); culling = isFilled
  end;

  list_vertex_count := list_vertex_count + 1;
end;

procedure TFormMatrix.vertex(pos: Vector3);
var
  triangle: array[1..3] of TPoint;
begin
  list_vertex[list_vertex_count] := pos;

  // Metode pembuatan segitiga yg digunakan adalah TRIANGLE_STRI
  if list_vertex_count > 0 then
  begin
     line(list_vertex[0], list_vertex[1]);
  end;

  if list_vertex_count > 1 then
  begin
    triangle[1] := ProjectTo2D(list_vertex[list_vertex_count-2]);
    triangle[2] := ProjectTo2D(list_vertex[list_vertex_count-1]);
    triangle[3] := ProjectTo2D(list_vertex[list_vertex_count]);
    Image1.Canvas.Polygon(triangle,true); // parameter (array of Tpoint, culling:Boolean); culling = isFilled
  end;

  list_vertex_count := list_vertex_count + 1;
end;
{%endregion}
{%endregion}

// EVENT HANDLING
procedure TFormMatrix.FormHide(Sender: TObject);
begin
  isShow := false;
end;

procedure TFormMatrix.FormShow(Sender: TObject);
begin
  isShow := true;
end;

// setup
procedure TFormMatrix.FormCreate(Sender: TObject);
begin
   CONST_M:= 500;
   clearCanvas();
   h:= Image1.Height;
   w:= Image1.Width;
   isShow := false;

   dt:= 0;       // untuk navigasi di sumbu y dari fungsi perlin
   dx:= 4;       // dx dipakai dalam customPerlinNoise untuk mapping 0 < x < w*4 ke 0..dx
   dy:= 4;       // dy dipakai dalam customPerlinNoise untuk mapping 0 < y < w*4 ke 0..dx
   sizeBox:= 50;
end;

// draw
procedure TFormMatrix.Timer1Timer(Sender: TObject);
var
  i, j: integer;
  y: double;
  point: Vector3;
  n: LongInt;
  red, green : Byte;
begin
  if isShow then
  begin
    clearCanvas();
    stroke(clBlack);

    n := Round(2 * (1+w*4/sizeBox)); // rumus perhitungan berapa banyak vertex yg dibuat berdasarkan ukuran size

    i:= -3000; // nilai asal
    while i < CONST_M do
    begin
      beginShape(n);

      j:= -w*2; // nilai asal
      while j <= w*2 do
      begin
        // 1
        y:= customPerlinNoise(j+w*2, i+3000); // Ditambah biar nilai input tidak negatif
        point:= rotateX(j, y*h-1000, i, 30);  // nilai 0..1 dimapping ke 0..height, lalu ditranslasi 1000 unit kebawah

        green := Round(255*y); // nilai 0..1 dimapping ke 0..255
        red := 255-green;
        fill(RGBToColor(red,green,0));
        vertex(point);

        // 2
        y:= customPerlinNoise(j+w*2, i+3000+sizeBox); // Ditambah biar nilai input tidak negatif
        point:= rotateX(j, y*h-1000, i+sizeBox, 30);  // nilai 0..1 dimapping ke 0..height, lalu ditranslasi 1000 unit kebawah

        green := Round(255*y); // nilai 0..1 dimapping ke 0..255
        red := 255-green;
        fill(RGBToColor(red,green,0));
        vertex(point);

        // increment
        j:= j+sizeBox;
      end;
      i:= i + sizeBox;
    end;
    dt:= dt + vt * 0.01;
  end;
end;

procedure TFormMatrix.TrackBarVtChange(Sender: TObject);
begin
  vt:= TrackBarVt.Position;
  EditVt.Text:= IntToStr(TrackBarVt.Position);
end;

procedure TFormMatrix.TrackBarDxChange(Sender: TObject);
begin
  dx:= TrackBarDx.Position;
  EditDx.Text:= IntToStr(TrackBarDx.Position);
end;

procedure TFormMatrix.TrackBarDzChange(Sender: TObject);
begin
  dy:= TrackBarDz.Position;
  EditDz.Text:= IntToStr(TrackBarDz.Position);
end;

procedure TFormMatrix.TrackBarSizeChange(Sender: TObject);
begin
  sizeBox:= TrackBarSize.Position;
  EditS.Text:= IntToStr(TrackBarSize.Position);
end;

end.

