unit PascalBoidsField;
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6018 off : unreachable code}
interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Dialogs, Graphics, LCLType, Boid;

const
  MAXIMUM_BOID_COUNT = 1000;
  DEFAULT_BOID_COUNT = 1;//00;

  //TODO: Add main form checkbox to determine bounce vs. wrap-around.
  BOUNCE_OFF_WALLS = true;

type
  TPascalBoidsField = class(TCustomControl)
    public
      CurrentBoidCount: Integer;

      procedure Initialize;
      procedure Randomize(const BoidCount: Integer);
      procedure Iterate;
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
      procedure AddNewBoid(const X: Integer; const Y: Integer; const MakeHawk: Boolean);
      procedure MouseDown(Sender: TObject; Button: TMouseButton;
        {%H-}Shift: TShiftState; X, Y: Integer); overload;
  end;

implementation

  var
    Boids: array[1..MAXIMUM_BOID_COUNT] of TBoid;

  procedure TPascalBoidsField.Initialize;
  var
    i: Integer;
    bi: TBoid;
  begin
    CurrentBoidCount := DEFAULT_BOID_COUNT;

    for i := 1 to MAXIMUM_BOID_COUNT do begin
      bi := TBoid.Create;
      //bi.Randomize(Width, Height);
      Boids[i] := bi;
    end;

    OnMouseDown := @MouseDown;
  end;

  procedure TPascalBoidsField.Randomize(const BoidCount: Integer);
  var
    i: Integer;
    bi: TBoid;
  begin
    CurrentBoidCount := BoidCount;

    for i := 1 to CurrentBoidCount do begin
      bi := Boids[i];
      bi.Randomize(Width, Height);
    end;
  end;

  procedure TPascalBoidsField.Iterate;
  var
    i: Integer;
    bi: TBoid;
    avoidHawk: Boolean;
  begin
    //TODO: Add dialog controls to adjust rule Distance and Power parameters.
    for i := 1 to CurrentBoidCount do begin
      bi := Boids[i];

      bi.Flock(Boids, i, CurrentBoidCount, 50, 0.0003);

      bi.Align(Boids, i, CurrentBoidCount, 50, 0.01);

      avoidHawk := false;
      bi.Avoid(avoidHawk, Boids, i, CurrentBoidCount, 20, 0.001);

      avoidHawk := true;
      bi.Avoid(avoidHawk, Boids, i, CurrentBoidCount, 150, 0.00005);

      bi.AdjustVelocity;
    end;

    for i := 1 to CurrentBoidCount do begin
      bi := Boids[i];
      bi.MoveForward(1.0, 5.0);
      if (BOUNCE_OFF_WALLS) then begin
        bi.BounceAwayFromWalls(Width, Height, 20.0);
      end else begin
        bi.WrapAround(Width, Height);
      end;
    end;
  end;

  procedure TPascalBoidsField.EraseBackground(DC: HDC);
  begin
    // Uncomment this to enable default background erasing
    //inherited EraseBackground(DC);
  end;

  procedure TPascalBoidsField.Paint;
  var
    i: Integer;
    bi: TBoid;
    Bitmap: TBitmap;
  begin
    Bitmap := TBitmap.Create;
    try
      { https://wiki.freepascal.org/Drawing_with_canvas }

      Bitmap.Height := Height;
      Bitmap.Width := Width;

      Bitmap.Canvas.Brush.Color := clAqua;
      Bitmap.Canvas.FillRect(0, 0, Width, Height);

      for i := 1 to CurrentBoidCount do begin
        bi := Boids[i];
        bi.Paint(Bitmap.Canvas);
      end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

  procedure TPascalBoidsField.AddNewBoid(const X: Integer; const Y: Integer; const MakeHawk: Boolean);
  var
    i: Integer;
    bi: TBoid;
  begin
    if (CurrentBoidCount < MAXIMUM_BOID_COUNT) then begin
      Inc(CurrentBoidCount);
      i := CurrentBoidCount;
      bi := Boids[i];
      bi.Initialize(X, Y);
      bi.IsHawk := MakeHawk;
    end else begin
      ShowMessage(Format('The maximum number of Boid slots (%d) has been reached.', [MAXIMUM_BOID_COUNT]));
    end;
  end;

  procedure TPascalBoidsField.MouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    truncX: Integer;
    truncY: Integer;
    makeHawk: Boolean;
  begin
    // Left click adds regular Boid; right click adds Hawk.
    makeHawk := (Button = mbRight);

    truncX := Trunc(X);
    truncY := Trunc(Y);

    AddNewBoid(truncX, truncY, makeHawk);

    Paint;
  end;

begin
end.
