unit Boid;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, Math, SysUtils;

type
  TBoid = Class
    private
    public
      IsActive: Boolean;
      Radius: Integer;
      Mass: Single;
      X: Single;
      Y: Single;
      VelocityX: Single;
      VelocityY: Single;
      AccelerationX: Single;
      AccelerationY: Single;

      //TODO: Add Boolean IsHawk.

      procedure Clear;
      procedure Randomize(const Width: Integer; const Height: Integer);
      procedure Initialize(const newX: Integer; const newY: Integer);
      function MergeIfAdjacent(const OtherBoid: TBoid): Boolean;
      procedure Accelerate(const OtherBoid: TBoid);
      procedure Move;
      procedure Paint(const TheCanvas: TCanvas);
  end;

implementation
  const
    INITIAL_RADIUS = 2;
    INITIAL_MASS = 4.0;
    GRAVITY = 10.0;

  procedure TBoid.Clear;
  begin
    IsActive := false;

    Radius := INITIAL_RADIUS;
    Mass := INITIAL_MASS;

    VelocityX := 0.0;
    VelocityY := 0.0;

    AccelerationX := 0.0;
    AccelerationY := 0.0;
  end;

  procedure TBoid.Randomize(const Width: Integer; const Height: Integer);
  begin
    Clear();

    X := 1.0 + Random(Width - 1);
    Y := 1.0 + Random(Height - 1);

    IsActive := true;
  end;

  procedure TBoid.Initialize(const newX: Integer; const newY: Integer);
  begin
    Clear();

    X := newX;
    Y := newY;

    IsActive := true;
  end;

  function TBoid.MergeIfAdjacent(const OtherBoid: TBoid): Boolean;
  var
    dx: Single;
    dy: Single;
    distanceSquared: Single;
    r: Single;
    rSquared: Single;
    combinedMomentumX: Single;
    combinedMomentumY: Single;
    isAdjacent: Boolean;
  begin
    dx := OtherBoid.X - X;
    dy := OtherBoid.Y - Y;
    distanceSquared := (dx * dx) + (dy * dy);

    r := Radius + OtherBoid.Radius;
    rSquared := (r * r);

    isAdjacent := (distanceSquared <= rSquared);
    if (isAdjacent) then begin
      X := (X + OtherBoid.X) / 2;
      Y := (Y + OtherBoid.Y) / 2;

      combinedMomentumX := (VelocityX * Mass) + (OtherBoid.VelocityX * OtherBoid.Mass);
      combinedMomentumY := (VelocityY * Mass) + (OtherBoid.VelocityY * OtherBoid.Mass);

      Mass := Mass + OtherBoid.Mass;

      VelocityX := combinedMomentumX / Mass;
      VelocityY := combinedMomentumY / Mass;

      Radius := Ceil(Sqrt(Mass));

      OtherBoid.Clear();
    end;

    Result := isAdjacent;
  end;

  procedure TBoid.Accelerate(const OtherBoid: TBoid);
  var
    dx: Single;
    dy: Single;
    distanceSquared: Single;
    accelerationMagnitude: Single;
    distanceMagnitude: Single;
    dxNormalized: Single;
    dyNormalized: Single;
  begin
    dx := OtherBoid.X - X;
    dy := OtherBoid.Y - Y;
    distanceSquared := (dx * dx) + (dy * dy);

    distanceMagnitude := Sqrt(DistanceSquared);
    dxNormalized := dx / distanceMagnitude;
    dyNormalized := dy / distanceMagnitude;

    accelerationMagnitude := (GRAVITY * OtherBoid.Mass) / distanceSquared;

    AccelerationX := AccelerationX + (accelerationMagnitude * dxNormalized);
    AccelerationY := AccelerationY + (accelerationMagnitude * dyNormalized);
  end;

  procedure TBoid.Move;
  begin
    VelocityX := VelocityX + AccelerationX;
    VelocityY := VelocityY + AccelerationY;

    X := X + VelocityX;
    Y := Y + VelocityY;
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

    //TODO: Use different color if IsHawk.
    TheCanvas.Pen.Color := clWhite;
    TheCanvas.Brush.Color := clWhite;

    TheCanvas.Polygon(p);
  end;

end.

