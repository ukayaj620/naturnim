unit starfield;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Math;

type

  { TFormStar }

  TFormStar = class(TForm)
    ed_kecepatan: TEdit;
    ed_jumlah: TEdit;
    Image1: TImage;
    lbl_kecepatan: TLabel;
    ed_radius: TEdit;
    lbl_radius: TLabel;
    lbl_jumlah: TLabel;
    tb_radius: TTrackBar;
    tb_jumlah: TTrackBar;
    Timer1: TTimer;
    tb_kecepatan: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tb_jumlahChange(Sender: TObject);
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
      Bintang = record
        pos: Vector3;
        warna: TColor;
        rotasi: Vector3;
        kec_rotasi:Vector3;
      end;

    var
      CONST_M: double;
      list_vertex: array of Vector3;
      list_vertex_count: Longint;
      isShow: Boolean;

      list_bintang: Array[1..500] of Bintang;
      kecepatan: double;
      jumlah: integer;
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
    procedure star(x: double; y: double; z: double; radius: double; rotation_x : double = 0; rotation_Y : double = 0; rotation_Z : double = 0);
    procedure star(pos: Vector3; radius: double; rotation:Vector3);
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

  if (z1 < CONST_M) and (z2 < CONST_M) and (CONST_M <> 0) then
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


  if (pos1.z < CONST_M) and (pos2.z < CONST_M) and (CONST_M <> 0) then
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

  list_vertex[list_vertex_count] := temp;
  list_vertex_count := list_vertex_count + 1;
end;

procedure TFormStar.vertex(pos: Vector3);
begin
  list_vertex[list_vertex_count] := pos;
  list_vertex_count := list_vertex_count + 1;
end;

procedure TFormStar.endShape();
var
  k : longInt;
begin
  if list_vertex_count > 0 then
  begin
     line(list_vertex[0], list_vertex[1]);
  end;

  if list_vertex_count > 1 then
  begin
    for k:=2 to list_vertex_count-1 do
    begin
      line(list_vertex[k], list_vertex[k-1]);
      line(list_vertex[k], list_vertex[k-2]);
    end;
  end;
end;

procedure TFormStar.star(x: double; y: double; z: double; radius: double; rotation_x : double = 0; rotation_Y : double = 0; rotation_Z : double = 0);
var
  pointBintang: array[1..10] of Vector3;
  pointMonitor: array[1..10] of TPoint;
  k: integer;
  deg : double;
  bigger_circle: double;
  temp : double;
  lebihM : boolean;
begin
  if z < CONST_M then
  begin
    bigger_circle := sin(degtorad(126))/sin(degtorad(18));

    deg := rotation_z;
    for k:=1 to 5 do
    begin
      pointBintang[k*2-1].x := x + cos(degtorad(deg)) * radius;
      pointBintang[k*2-1].y := y + sin(degtorad(deg)) * radius;
      pointBintang[k*2-1].z := z;

      deg := deg + 36;

      pointBintang[k*2].x := x + cos(degtorad(deg)) * radius * bigger_circle;
      pointBintang[k*2].y := y + sin(degtorad(deg)) * radius * bigger_circle;
      pointBintang[k*2].z := z;

      deg := deg + 36;
    end;

    for k:=1 to 10 do
    begin
      // translasi ke pivot
      pointBintang[k].x := pointBintang[k].x - x;
      pointBintang[k].y := pointBintang[k].y - y;
      pointBintang[k].z := pointBintang[k].z - z;

      // rotasi Sumbu X
      temp := pointBintang[k].y;
      pointBintang[k].y := pointBintang[k].y * cos(degtorad(rotation_x)) - pointBintang[k].z * sin(degtorad(rotation_x));
      pointBintang[k].z :=      temp         * sin(degtorad(rotation_x)) + pointBintang[k].z * cos(degtorad(rotation_x));

      // rotasi Sumbu Y
      temp := pointBintang[k].x;
      pointBintang[k].x := pointBintang[k].x * cos(degtorad(rotation_y)) - pointBintang[k].z * sin(degtorad(rotation_y));
      pointBintang[k].z :=      temp         * sin(degtorad(rotation_y)) + pointBintang[k].z * cos(degtorad(rotation_y));

      // translasi kembali
      pointBintang[k].x := pointBintang[k].x + x;
      pointBintang[k].y := pointBintang[k].y + y;
      pointBintang[k].z := pointBintang[k].z + z;

      // proyeksi
      pointMonitor[k] := projectTo2D(pointBintang[k]);
    end;

    // cek apakah ada titik Z yg melebihi M
    lebihM := false;
    for k:=1 to 10 do
    begin
      if pointBintang[k].z >= CONST_M then
         lebihM := true;
    end;

    if lebihM = false then
       Image1.Canvas.Polygon(pointMonitor,true);
  end;
end;

procedure TFormStar.star(pos: Vector3; radius: double; rotation: Vector3);
var
  pointBintang: array[1..10] of Vector3;
  pointMonitor: array[1..10] of TPoint;
  k: integer;
  deg : double;
  bigger_circle: double;
  temp : double;
  lebihM : boolean;
begin
  if pos.z < CONST_M then
  begin
    bigger_circle := sin(degtorad(126))/sin(degtorad(18));

    deg := rotation.z;
    for k:=1 to 5 do
    begin
      pointBintang[k*2-1].x := pos.x + cos(degtorad(deg)) * radius;
      pointBintang[k*2-1].y := pos.y + sin(degtorad(deg)) * radius;
      pointBintang[k*2-1].z := pos.z;

      deg := deg + 36;

      pointBintang[k*2].x := pos.x + cos(degtorad(deg)) * radius * bigger_circle;
      pointBintang[k*2].y := pos.y + sin(degtorad(deg)) * radius * bigger_circle;
      pointBintang[k*2].z := pos.z;

      deg := deg + 36;
    end;

    for k:=1 to 10 do
    begin
      // translasi ke pivot
      pointBintang[k].x := pointBintang[k].x - pos.x;
      pointBintang[k].y := pointBintang[k].y - pos.y;
      pointBintang[k].z := pointBintang[k].z - pos.z;

      // rotasi Sumbu X
      temp := pointBintang[k].y;
      pointBintang[k].y := pointBintang[k].y * cos(degtorad(rotation.x)) - pointBintang[k].z * sin(degtorad(rotation.x));
      pointBintang[k].z :=      temp         * sin(degtorad(rotation.x)) + pointBintang[k].z * cos(degtorad(rotation.x));

      // rotasi Sumbu Y
      temp := pointBintang[k].x;
      pointBintang[k].x := pointBintang[k].x * cos(degtorad(rotation.y)) - pointBintang[k].z * sin(degtorad(rotation.y));
      pointBintang[k].z :=      temp         * sin(degtorad(rotation.y)) + pointBintang[k].z * cos(degtorad(rotation.y));

      // translasi kembali
      pointBintang[k].x := pointBintang[k].x + pos.x;
      pointBintang[k].y := pointBintang[k].y + pos.y;
      pointBintang[k].z := pointBintang[k].z + pos.z;

      // proyeksi
      pointMonitor[k] := projectTo2D(pointBintang[k]);
    end;

    // cek apakah ada titik Z yg melebihi M
    lebihM := false;
    for k:=1 to 10 do
    begin
      if pointBintang[k].z >= CONST_M then
         lebihM := true;
    end;

    if lebihM = false then
       Image1.Canvas.Polygon(pointMonitor,true);
  end;
end;
{%endregion}
{%endregion}


// EVENT HANDLING

procedure TFormStar.tb_kecepatanChange(Sender: TObject);
begin
  ed_kecepatan.text := inttostr(tb_kecepatan.Position);
end;

procedure TFormStar.tb_radiusChange(Sender: TObject);
begin
  ed_radius.text := inttostr(tb_radius.Position);
end;

procedure TFormStar.tb_jumlahChange(Sender: TObject);
var
  k, index : integer;
  temp : Bintang;
begin
  // shuffle agar bintang yg berkurang atau bertambah adalah random
  for k:=1 to jumlah do
  begin
    index := Random(jumlah)+1;

    temp := list_bintang[index];
    list_bintang[index] := list_bintang[k];
    list_bintang[k] := temp;
  end;

  jumlah := tb_jumlah.Position * 10;
  ed_jumlah.text := inttostr(jumlah);
end;

procedure TFormStar.FormHide(Sender: TObject);
begin
  isShow := false;
end;

procedure TFormStar.FormShow(Sender: TObject);
begin
  isShow := true;
end;

// Setup
procedure TFormStar.FormCreate(Sender: TObject);
var
  k, j : integer;
  index : Bintang;
begin
  CONST_M := 200;
  kecepatan := 0;
  isShow := false;

  Randomize();
  for k:=1 to 500 do
  begin
    list_bintang[k].pos.x := -2000 + Random() * 4000;
    list_bintang[k].pos.y := -2000 + Random() * 4000;
    list_bintang[k].pos.z := -2000 + Random() * 2000;
    list_bintang[k].warna := TColor(Random(16777215)); // #FFFFFF
    list_bintang[k].rotasi.x := 0;
    list_bintang[k].rotasi.y := 0;
    list_bintang[k].rotasi.z := Random(360);
    list_bintang[k].kec_rotasi.x := -1 + Random(3);
    list_bintang[k].kec_rotasi.y := -1 + Random(3);
    list_bintang[k].kec_rotasi.z := 0;
  end;

  // Insertion Sort berdasarkan posisi z
  for k:=2 to jumlah do
  begin
    index := list_bintang[k];
    j := k;
    while ((j > 1) AND (list_bintang[j-1].pos.z > index.pos.z)) do
    begin
      list_bintang[j] := list_bintang[j-1];
      j := j-1;
    end;
    list_bintang[j] := index;
  end;

end;

// draw
procedure TFormStar.Timer1Timer(Sender: TObject);
var
  k, j : integer;
  index : Bintang;
  titik: Vector3;
  ketebalan: double;
begin
  if isShow then
  begin
    // update
    jumlah := strtoint(ed_jumlah.Text);
    kecepatan := tb_Kecepatan.Position;

    for k:=1 to jumlah do
    begin
      list_bintang[k].kec_rotasi.z := kecepatan;

      list_bintang[k].pos.z := list_bintang[k].pos.z + kecepatan;
      list_bintang[k].rotasi.x := list_bintang[k].rotasi.x + list_bintang[k].kec_rotasi.x;
      list_bintang[k].rotasi.y := list_bintang[k].rotasi.y + list_bintang[k].kec_rotasi.y;
      list_bintang[k].rotasi.z := list_bintang[k].rotasi.z + list_bintang[k].kec_rotasi.z;

      // logic
      if list_bintang[k].pos.z > CONST_M then
      begin
         list_bintang[k].pos.x := -2000 + Random() * 4000;
         list_bintang[k].pos.y := -2000 + Random() * 4000;
         list_bintang[k].pos.z := list_bintang[k].pos.z - 2000 - CONST_M;
         list_bintang[k].warna := TColor(Random(16777215)); // #FFFFFF
         list_bintang[k].rotasi.z := Random(360);
      end;

      if list_bintang[k].pos.z < -2000 then
      begin
         list_bintang[k].pos.x := -2000 + Random() * 4000;
         list_bintang[k].pos.y := -2000 + Random() * 4000;
         list_bintang[k].pos.z := list_bintang[k].pos.z + 2000 + CONST_M;
         list_bintang[k].warna := TColor(Random(16777215)); // #FFFFFF
         list_bintang[k].rotasi.z := Random(360);
      end;
    end;

    // Insertion Sort berdasarkan posisi z
    for k:=2 to jumlah do
    begin
      index := list_bintang[k];
      j := k;
      while ((j > 1) AND (list_bintang[j-1].pos.z > index.pos.z)) do
      begin
        list_bintang[j] := list_bintang[j-1];
        j := j-1;
      end;
      list_bintang[j] := index;
    end;

    // draw
    clearCanvas();

    for k:=1 to jumlah do
    begin
      titik := list_bintang[k].pos;

      ketebalan := 0.25*tb_radius.Position / (1-titik.z/CONST_M);
      if ketebalan > 50 then
         ketebalan := 50;

      stroke(list_bintang[k].warna);
      StrokeWeight(ketebalan);
      fill(clWhite);

      line(titik.x, titik.y, titik.z, titik.x, titik.y, titik.z - 4*kecepatan);
      star(titik, tb_radius.Position, list_bintang[k].rotasi);;
    end;
  end;
end;


{$R *.lfm}

end.

