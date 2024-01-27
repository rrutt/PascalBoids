unit Boid;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, SysUtils;

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

      procedure Clear;
      procedure Randomize(const Width: Integer; const Height: Integer);
      procedure Initialize(const newX: Integer; const newY: Integer);
      function MergeIfAdjacent(const OtherBoid: TBoid): Boolean;
      procedure Accelerate(const OtherBoid: TBoid);
      procedure Move;
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

    X := 1.0 + Random(Width - 1) - (Width div 2);
    Y := 1.0 + Random(Height - 1) - (Height div 2);

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

end.

