unit Boid;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 5029 off : Private field "$1.$2" is never used}
interface

uses
  Classes, Graphics, SysUtils;

type
  TBoid = Class
    private
      FlockX: Single;
      FlockY: Single;
      AlignX: Single;
      AlignY: Single;
      AvoidX: Single;
      AvoidY: Single;
      AvoidHawkX: Single;
      AvoidHawkY: Single;
    public
      IsHawk: Boolean;
      X: Single;
      Y: Single;
      VelocityX: Single;
      VelocityY: Single;

      procedure Clear;
      procedure Randomize(const Width: Integer; const Height: Integer);
      procedure Initialize(const NewX: Single; const NewY: Single);
      procedure Paint(const TheCanvas: TCanvas);
      procedure Flock(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
      procedure Align(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
      procedure Avoid(const AvoidHawk: Boolean; const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
      procedure AdjustVelocity;
      procedure MoveForward(const MinSpeed: Single; const MaxSpeed: Single);
      procedure BounceAwayFromWalls(const Width: Integer; const Height: Integer; const Pad: Single);
      procedure WrapAround(const Width: Integer; const Height: Integer);
  end;

implementation

  procedure TBoid.Clear;
  begin
    IsHawk := false;;

    VelocityX := 0.0;
    VelocityY := 0.0;
  end;

  procedure TBoid.Randomize(const Width: Integer; const Height: Integer);
  var
    randomX: Single;
    randomY: Single;
  begin
    Clear();

    randomX := 1.0 + Random(Width - 1);
    randomY := 1.0 + Random(Height - 1);

    X := randomX;
    Y := randomY;

    IsHawk := false;
  end;

  procedure TBoid.Initialize(const NewX: Single; const NewY: Single);
  begin
    Clear();

    X := NewX;
    Y := NewY;

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
    // Steer Toward Center of Mass of Nearby Boids.
    (*
        var neighbors = Boids.Where(x => x.GetDistance(boid) < distance);
        double meanX = neighbors.Sum(x => x.X) / neighbors.Count();
        double meanY = neighbors.Sum(x => x.Y) / neighbors.Count();
        double deltaCenterX = meanX - boid.X;
        double deltaCenterY = meanY - boid.Y;
        return (deltaCenterX * power, deltaCenterY * power);
    *)
    i := SelfIndex;
    bi := Boids[i];

    bi.FlockX := 0.0;
    bi.FlockY := 0.0;

    distanceThresholdSquared := DistanceThreshold * DistanceThreshold;
    sumX := 0.0;
    sumY := 0.0;

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
      bi.FlockX := (meanX - bi.X) * Power;
      bi.FlockY := (meanY - bi.Y) * Power;
    end;
  end;

  procedure TBoid.Align(const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
  var
    distanceThresholdSquared: Single;
    distanceSquared: Single;
    neighborCount: Integer;
    distanceX: Single;
    distanceY: Single;
    sumVelocityX: Single;
    sumVelocityY: Single;
    meanVelocityX: Single;
    meanVelocityY: Single;
    deltaVelocityX: Single;
    deltaVelocityY: Single;
    i: Integer;
    j: Integer;
    bi: Tboid;
    bj: TBoid;
  begin
    // Mimic Direction and Speed of Nearby Boids.
    (*
        var neighbors = Boids.Where(x => x.GetDistance(boid) < distance);
        double meanXvel = neighbors.Sum(x => x.Xvel) / neighbors.Count();
        double meanYvel = neighbors.Sum(x => x.Yvel) / neighbors.Count();
        double dXvel = meanXvel - boid.Xvel;
        double dYvel = meanYvel - boid.Yvel;
        return (dXvel * power, dYvel * power);
    *)
    i := SelfIndex;
    bi := Boids[i];

    bi.AlignX := 0.0;
    bi.AlignY := 0.0;

    distanceThresholdSquared := DistanceThreshold * DistanceThreshold;
    sumVelocityX := 0.0;
    sumVelocityY := 0.0;

    neighborCount := 0;
    for j := 1 to BoidCount do begin
      bj := Boids[j];
      if ((j <> i) and (not bj.IsHawk)) then begin
        distanceX := bj.X - bi.X;
        distanceY := bj.Y - bi.Y;
        distanceSquared := (distanceX * distanceX) + (distanceY * distanceY);
        if (distanceSquared < distanceThresholdSquared) then begin
          Inc(neighborCount);
          sumVelocityX := sumVelocityX + bj.X;
          sumVelocityY := sumVelocityY + bj.Y;
        end;
      end;
    end;

    if (neighborCount > 0) then begin
      meanVelocityX := sumVelocityX / neighborCount;
      meanVelocityY := sumVelocityY / neighborCount;

      deltaVelocityX := meanVelocityX - bi.X;
      deltaVelocityY := meanVelocityY - bi.Y;

      bi.AlignX := deltaVelocityX * Power;
      bi.AlignY := deltaVelocityY * Power;
    end;
  end;

  procedure TBoid.Avoid(const AvoidHawk: Boolean; const Boids: array of TBoid; const SelfIndex: Integer; const BoidCount: Integer; const DistanceThreshold: Integer; const Power: Single);
  var
    distanceThresholdSquared: Single;
    distanceSquared: Single;
    distanceX: Single;
    distanceY: Single;
    closeness: Single;
    sumClosenessX: Single;
    sumClosenessY: Single;
    i: Integer;
    j: Integer;
    bi: Tboid;
    bj: TBoid;
    biIsHawk: Boolean;
    bjIsHawk: Boolean;
    ix: Single;
    iy: Single;
    jx: Single;
    jy: Single;
  begin
    // Steer Away from Extremely Close Boids.
    (*
        var neighbors = Boids.Where(x => x.GetDistance(boid) < distance);
        (double sumClosenessX, double sumClosenessY) = (0, 0);
        foreach (var neighbor in neighbors)
        {
            double closeness = distance - boid.GetDistance(neighbor);
            sumClosenessX += (boid.X - neighbor.X) * closeness;
            sumClosenessY += (boid.Y - neighbor.Y) * closeness;
        }
        return (sumClosenessX * power, sumClosenessY * power);
    *)
    i := SelfIndex;
    bi := Boids[i];
    biIsHawk := bi.IsHawk;

    distanceThresholdSquared := DistanceThreshold * DistanceThreshold;
    sumClosenessX := 0.0;
    sumClosenessY := 0.0;

    if (not biIsHawk) then begin
      for j := 1 to BoidCount do begin
        bj := Boids[j];
        bjIsHawk := bj.IsHawk;

        ix := bi.X;
        iy := bi.Y;

        jx := bj.X;
        jy := bj.Y;

        distanceX := jx - ix;
        distanceY := jy - iy;

        distanceSquared := (distanceX * distanceX) + (distanceY * distanceY);
        if ((j <> i) and (AvoidHawk = bjIsHawk)) then begin
          if (distanceSquared < distanceThresholdSquared) then begin
            closeness := DistanceThreshold - Sqrt(distanceSquared);
            sumClosenessX := (bi.X - bj.X) * closeness;
            sumClosenessY := (bi.Y - bj.Y) * closeness;
          end;
        end;
      end;
    end;

    if (AvoidHawk) then begin
      bi.AvoidHawkX := sumClosenessX * Power;
      bi.AvoidHawkY := sumClosenessY * Power;
    end else begin
      bi.AvoidX := sumClosenessX * Power;
      bi.AvoidY := sumClosenessY * Power;
    end;
  end;

  procedure TBoid.AdjustVelocity;
  begin
    VelocityX := VelocityX + FlockX + AlignX + AvoidX + AvoidHawkX;
    VelocityY := VelocityY + FlockY + AlignY + AvoidY + AvoidHawkY;
  end;

  procedure TBoid.MoveForward(const MinSpeed: Single; const MaxSpeed: Single);
  var
    speed: Single;
  begin
    // Speed Limit.
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

    speed := Sqrt((VelocityX * VelocityX) + (VelocityY * VelocityY));
    if (speed > MaxSpeed) then begin
      VelocityX := (VelocityX / speed) * MaxSpeed;
      VelocityY := (VelocityY / speed) * MaxSpeed;
    end else if (speed < minSpeed) then begin
      if (speed > 0.0) then begin
        VelocityX := (VelocityX / speed) * MinSpeed;
        VelocityY := (VelocityY / speed) * MinSpeed;
      end else begin
        VelocityX := 0.1;
      end;
    end;
  end;

  procedure TBoid.BounceAwayFromWalls(const Width: Integer; const Height: Integer; const Pad: Single);
  begin
    // Avoid Edges.
    if ((X < Pad) and (VelocityX < 0)) then begin
      VelocityX := -VelocityX;
    end;
    if ((X > (Width - Pad)) and (VelocityX > 0)) then begin
      VelocityX := -VelocityX;
    end;
    if ((Y < Pad) and (VelocityY < 0)) then begin
      VelocityY := -VelocityY;
    end;
    if ((Y > (Height - Pad)) and (VelocityY > 0)) then begin
      VelocityY := -VelocityY;
    end;
  end;

  procedure TBoid.WrapAround(const Width: Integer; const Height: Integer);
  begin
    // Wrap the Universe.
    if (X < 0) then begin
      X := X + Width;
    end;
    if (X > Width) then begin
      X := X - Width;
    end;
    if (Y < 0) then begin
      Y := Y + Height;
    end;
    if (Y > Height) then begin
      Y := Y - Height;
    end;
  end;

end.

