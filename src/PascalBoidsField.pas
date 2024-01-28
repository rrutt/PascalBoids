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
      InitialBoidCount: Integer;
      CenterX: Integer;
      CenterY: Integer;

    public
      ActiveBoidCount: Integer;

      procedure Initialize;
      procedure Randomize(const BoidCount: Integer);
      procedure Iterate;
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
      procedure AddNewBoid(const X: Integer; const Y: Integer);
      procedure MouseDown(Sender: TObject; Button: TMouseButton;
        {%H-}Shift: TShiftState; X, Y: Integer); overload;
  end;

implementation

  var
    Boids: array[1..MAXIMUM_BOID_COUNT] of TBoid;

  procedure TPascalBoidsField.Initialize;
  var
    i: Integer;
    a: TBoid;
  begin
    InitialBoidCount := MAXIMUM_BOID_COUNT;

    CenterX := Width div 2;
    CenterY := Height div 2;

    for i := 1 to MAXIMUM_BOID_COUNT do begin
      a := TBoid.Create;
      Boids[i] := a;
    end;

    ActiveBoidCount := 0;

    OnMouseDown := @MouseDown;
  end;

  procedure TPascalBoidsField.Randomize(const BoidCount: Integer);
  var
    i: Integer;
  begin
    InitialBoidCount := BoidCount;

    for i := 1 to InitialBoidCount do begin
      Boids[i].Randomize(Width, Height);
    end;

    ActiveBoidCount := InitialBoidCount;
  end;

  procedure TPascalBoidsField.Iterate;
  var
    i: Integer;
    bi: TBoid;
  begin
    //TODO: Implement rules iteration per https://swharden.com/csdv/simulations/boids/

(*
    // update void speed and direction (velocity) based on rules
    foreach (var boid in Boids)
    {
        (double flockXvel, double flockYvel) = Flock(boid, 50, .0003);
        (double alignXvel, double alignYvel) = Align(boid, 50, .01);
        (double avoidXvel, double avoidYvel) = Avoid(boid, 20, .001);
        (double predXvel, double predYval) = Predator(boid, 150, .00005);
        boid.Xvel += flockXvel + avoidXvel + alignXvel + predXvel;
        boid.Yvel += flockYvel + avoidYvel + alignYvel + predYval;
    }

    // move all boids forward in time
    foreach (var boid in Boids)
    {
        boid.MoveForward();
        if (bounceOffWalls)
            BounceOffWalls(boid);
        if (wrapAroundEdges)
            WrapAround(boid);
    }
*)
    for i := 1 to InitialBoidCount do begin
      bi := Boids[i];
      if (bi.IsActive) then begin
        bi := Boids[i];
        bi.Flock(Boids, 50, 0.0003);
        bi.Align(Boids, 50, 0.01);
        bi.Avoid(Boids, 20, 0.001);
        bi.Predator(Boids, 150, 0.00005);
        bi.AdjustVelocity;
      end;
    end;

    for i := 1 to InitialBoidCount do begin
      bi := Boids[i];
      if (bi.IsActive) then begin
        bi.MoveForward;
        if (BOUNCE_OFF_WALLS) then begin
            bi.BounceAwayFromWalls;
        end;
        if (WRAP_AROUND_EDGES) then begin
            bi.WrapAround;
        end;
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
    a: TBoid;
    Bitmap: TBitmap;
  begin
    Bitmap := TBitmap.Create;
    try
      { https://wiki.freepascal.org/Drawing_with_canvas }

      Bitmap.Height := Height;
      Bitmap.Width := Width;

      Bitmap.Canvas.Brush.Color := clAqua;
      Bitmap.Canvas.FillRect(0, 0, Width, Height);

      for i := 1 to InitialBoidCount do begin
        a := Boids[i];
        if (a.IsActive) then begin
          a.Paint(Bitmap.Canvas);
        end;
      end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

  procedure TPascalBoidsField.AddNewBoid(const X: Integer; const Y: Integer);
  var
    reactivatedBoid: Boolean;
    i: Integer;
    ai: TBoid;
  begin
    //TODO: Support creation of Hawk predators.

    reactivatedBoid := false;

    for i := 1 to InitialBoidCount do begin
      ai := Boids[i];
      if (not reactivatedBoid) and (not ai.IsActive) then begin
        ai.Initialize(X, Y);
        Inc(ActiveBoidCount);
        reactivatedBoid := true;
      end;
    end;

    if (not reactivatedBoid) then begin
      if (InitialBoidCount < MAXIMUM_BOID_COUNT) then begin
        i := InitialBoidCount + 1;
        ai := Boids[i];
        ai.Initialize(X, Y);

        Inc(InitialBoidCount);
        Inc(ActiveBoidCount);
      end else begin
        ShowMessage(Format('The maximum number of Boid slots (%d) has been reached.', [MAXIMUM_BOID_COUNT]));
      end;
    end;
  end;

  procedure TPascalBoidsField.MouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    truncX: Integer;
    truncY: Integer;
  begin
    //TODO: Left click adds regular Boid; right click adds Hawk.
    truncX := Trunc(X);
    truncY := Trunc(Y);

    AddNewBoid(truncX, truncY);

    Paint;
  end;

begin
end.
