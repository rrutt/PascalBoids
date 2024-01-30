unit Boid;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5029 off : Private field "$1.$2" is never used}
interface

uses
  Classes, Graphics, Math, SysUtils;

type
  TBoid = Class
    private
      FlockX: Single;
      FlockY: Single;
      AlignX: Single;
      AlignY: Single;
      AvoidX: Single;
      AvoidY: Single;
      PredatorX: Single;
      PredatorY: Single;
    public
      IsHawk: Boolean;
      X: Single;
      Y: Single;
      VelocityX: Single;
      VelocityY: Single;

      procedure Clear;
      procedure Randomize(const Width: Integer; const Height: Integer);
      procedure Initialize(const newX: Integer; const newY: Integer);
      procedure Paint(const TheCanvas: TCanvas);
      procedure Flock(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
      procedure Align(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
      procedure Avoid(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
      procedure Predator(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
      procedure AdjustVelocity;
      procedure MoveForward;
      procedure BounceAwayFromWalls;
      procedure WrapAround;
  end;

implementation

  procedure TBoid.Clear;
  begin
    IsHawk := false;;

    VelocityX := 0.0;
    VelocityY := 0.0;
  end;

  procedure TBoid.Randomize(const Width: Integer; const Height: Integer);
  begin
    Clear();

    X := 1.0 + Random(Width - 1);
    Y := 1.0 + Random(Height - 1);

    IsHawk := false;
  end;

  procedure TBoid.Initialize(const newX: Integer; const newY: Integer);
  begin
    Clear();

    X := newX;
    Y := newY;

    IsHawk := false;
  end;

  procedure TBoid.Paint(const TheCanvas: TCanvas);
  var
    truncX: Integer;
    truncY: Integer;
    p: Array[0..4] of TPoint;
  begin
    truncX := Trunc(X);
    truncY := Trunc(Y);

    if (Abs(VelocityY) >= Abs(VelocityX)) then begin
      if (VelocityY >= 0) then begin
        p[0].X := truncX + 0; p[0].Y := truncY + 0;
        p[1].X := truncX - 5; p[1].Y := truncY - 1;
        p[2].X := truncX + 0; p[2].Y := truncY + 10;
        p[3].X := truncX + 5; p[3].Y := truncY - 1;
        p[4].X := truncX + 0; p[4].Y := truncY + 0;
      end else begin
        p[0].X := truncX + 0; p[0].Y := truncY + 0;
        p[1].X := truncX + 5; p[1].Y := truncY + 1;
        p[2].X := truncX + 0; p[2].Y := truncY - 10;
        p[3].X := truncX - 5; p[3].Y := truncY + 1;
        p[4].X := truncX + 0; p[4].Y := truncY + 0;
      end
    end else begin
      if (VelocityX >= 0) then begin
        p[0].Y := truncY + 0; p[0].X := truncX + 0;
        p[1].Y := truncY - 5; p[1].X := truncX - 1;
        p[2].Y := truncY + 0; p[2].X := truncX + 10;
        p[3].Y := truncY + 5; p[3].X := truncX - 1;
        p[4].Y := truncY + 0; p[4].X := truncX + 0;
      end else begin
        p[0].Y := truncY + 0; p[0].X := truncX + 0;
        p[1].Y := truncY + 5; p[1].X := truncX + 1;
        p[2].Y := truncY + 0; p[2].X := truncX - 10;
        p[3].Y := truncY - 5; p[3].X := truncX + 1;
        p[4].Y := truncy + 0; p[4].X := truncX + 0;
      end;
    end;

    if (IsHawk) then begin
      TheCanvas.Pen.Color := clMaroon;
      TheCanvas.Brush.Color := clMaroon;
    end else begin
      TheCanvas.Pen.Color := clWhite;
      TheCanvas.Brush.Color := clWhite;
    end;

    TheCanvas.Polygon(p);
  end;

  procedure TBoid.Flock(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
  var
    distanceThresholdSquared: Single;
    distanceSquared: Single;
    neighborCount: Integer;
    distanceX: Single;
    distanceY: Single;
    sumX: Single;
    sumY: Single;
    meanX: Single;
    meanY: Single;
    i: Integer;
    j: Integer;
    bi: Tboid;
    bj: TBoid;
  begin

    (*
        var neighbors = Boids.Where(x => x.GetDistance(boid) < distance);
        double meanX = neighbors.Sum(x => x.X) / neighbors.Count();
        double meanY = neighbors.Sum(x => x.Y) / neighbors.Count();
        double deltaCenterX = meanX - boid.X;
        double deltaCenterY = meanY - boid.Y;
        return (deltaCenterX * power, deltaCenterY * power);
    *)
    FlockX := 0;
    FlockY := 0;

    distanceThresholdSquared := DistanceThreshold * DistanceThreshold;
    sumX := 0.0;
    sumY := 0.0;

    i := SelfIndex;
    bi := Boids[i];

    neighborCount := 0;
    for j := 1 to BoidCount do begin
      bj := Boids[j];
      if ((j <> i) and (not bj.IsHawk)) then begin
        distanceX := bj.X - bi.X;
        distanceY := bj.Y - bi.Y;
        distanceSquared := (distanceX * distanceX) + (distanceY * distanceY);
        if (distanceSquared < distanceThresholdSquared) then begin
          Inc(neighborCount);
          sumX := sumX + bj.X;
          sumY := sumY + bj.Y;
        end;
      end;
    end;

    if (neighborCount > 0) then begin
      meanX := sumX / neighborCount;
      meanY := sumY / neighborCount;
      FlockX := (meanX - X) * Power;
      FlockY := (meanY - Y) * Power;
    end;
  end;

  procedure TBoid.Align(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
  begin
    //TODO: Align.
    AlignX := 0;
    AlignY := 0;
  end;

  procedure TBoid.Avoid(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
  begin
    //TODO: Avoid.
    AvoidX := 0;
    AvoidY := 0;
  end;

  procedure TBoid.Predator(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
  begin
    //TODO: Predator.
    PredatorX := 0;
    PredatorY := 0;
  end;

  procedure TBoid.AdjustVelocity;
  begin
    VelocityX := VelocityX + FlockX + AlignX + AvoidX + PredatorX;
    VelocityY := VelocityY + FlockY + AlignY + AvoidY + PredatorY;
  end;

  procedure TBoid.MoveForward;
  begin
    (*
        X += Xvel;
        Y += Yvel;

        var speed = GetSpeed();
        if (speed > maxSpeed)
        {
            Xvel = (Xvel / speed) * maxSpeed;
            Yvel = (Yvel / speed) * maxSpeed;
        }
        else if (speed < minSpeed)
        {
            Xvel = (Xvel / speed) * minSpeed;
            Yvel = (Yvel / speed) * minSpeed;
        }

        if (double.IsNaN(Xvel))
            Xvel = 0;
        if (double.IsNaN(Yvel))
            Yvel = 0;
    *)

    X := X + VelocityX;
    Y := Y + VelocityY;

    //TODO: Add minimum & maximum speed to MoveForward.
  end;

  procedure TBoid.BounceAwayFromWalls;
  begin
    //TODO: BounceAwayFromWalls;
  end;

  procedure TBoid.WrapAround;
  begin
    //TODO: WrapAround.
  end;

end.

