unit PascalBoidsField;
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6018 off : unreachable code}
interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Dialogs, Graphics, LCLType, Boid;

const
  MAXIMUM_BOID_COUNT = 1000;
  DEFAULT_BOID_COUNT = 100;

  BOUNCE_OFF_WALLS = true;
  WRAP_AROUND_EDGES = false;

  //TODO: Add hawk predator count.

type
  TPascalBoidsField = class(TCustomControl)
    private
      CenterX: Integer;
      CenterY: Integer;

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

    CenterX := Width div 2;
    CenterY := Height div 2;

    for i := 1 to MAXIMUM_BOID_COUNT do begin
      bi := TBoid.Create;
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
  begin
    for i := 1 to CurrentBoidCount do begin
      bi := Boids[i];
      bi := Boids[i];
      bi.Flock(Boids, 50, 0.0003);
      bi.Align(Boids, 50, 0.01);
      bi.Avoid(Boids, 20, 0.001);
      bi.Predator(Boids, 150, 0.00005);
      bi.AdjustVelocity;
    end;

    for i := 1 to CurrentBoidCount do begin
      bi := Boids[i];
      bi.MoveForward;
      if (BOUNCE_OFF_WALLS) then begin
          bi.BounceAwayFromWalls;
      end;
      if (WRAP_AROUND_EDGES) then begin
          bi.WrapAround;
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
    //TODO: Support creation of Hawk predators.

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
