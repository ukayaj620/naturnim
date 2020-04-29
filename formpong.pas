unit FormPong;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TSpacePong }

  TSpacePong = class(TForm)
    Ball: TShape;
    lblScore: TLabel;
    lblgameover: TLabel;
    lblrestart: TLabel;
    Paddle: TShape;
    tmrGame: TTimer;
    procedure ControlPaddle(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure lblrestartClick(Sender: TObject);
    procedure PaddleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure tmrGameTimer(Sender: TObject);
  private
    procedure Initgame;
    procedure UpdateScore;
    procedure gameover;
    procedure IncreaseSpeed;
  public

  end;

var
  SpacePong: TSpacePong;
  Score: integer;
  SpeedX, SpeedY: integer;

implementation

{$R *.lfm}

{ TSpacePong }

procedure TSpacePong.ControlPaddle(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  Paddle.Left:=X-Paddle.Width div 2;
  Paddle.Top:=ClientHeight-Paddle.Height-2;
end;

procedure TSpacePong.FormCreate(Sender: TObject);
begin
  DoubleBuffered:=True;
  InitGame;

end;

procedure TSpacePong.lblrestartClick(Sender: TObject);
begin
  InitGame;
end;

procedure TSpacePong.PaddleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  ControlPaddle(Sender,Shift,X+Paddle.Left,Y+Paddle.Top);
end;

procedure TSpacePong.tmrGameTimer(Sender: TObject);
begin
  Ball.Left:=Ball.Left+SpeedX;
  Ball.Top:=Ball.Top+SpeedY;

  if Ball.Top <= 0 then SpeedY:= -SpeedY;
  if (Ball.Left <= 0) or (Ball.Left + Ball.Width >= ClientWidth) then SpeedX:=-SpeedX;

  if Ball.Top + Ball.Height >= ClientHeight then gameover;

  if (Ball.Left + Ball.Width >= Paddle.Left) and (Ball.Left <= Paddle.Left + Paddle.Width)
   and (Ball.Top + Ball. Height >= Paddle.Top) then

begin
   SpeedY:=-SpeedY;
   IncreaseSpeed;

   Inc(Score);
   UpdateScore
end;
end;

procedure TSpacePong.InitGame;
begin
  SpeedX:=5;
  SpeedY:=5;
  Score:=0;

  lblgameover.Visible:=False;
  lblrestart.Visible:=False;

  Ball.Left:=10;
  Ball.Top:=10;

  UpdateScore;
  tmrGame.Enabled:=True;
end;

procedure TSpacePong.UpdateScore;
begin
  lblScore.Caption:='Score: ' + IntToStr(Score);
end;

procedure TSpacePong.gameover;
begin
   tmrGame.Enabled:=False;

   lblgameover.Visible:=True;
   lblrestart.Visible:=True;
end;

procedure TSpacePong.IncreaseSpeed;
begin
  if SpeedX > 0 then Inc(SpeedX) else Dec(SpeedX);
  if SpeedY > 0 then Inc(SpeedY) else Dec(SpeedY);
end;

end.

