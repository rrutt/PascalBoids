unit PascalBoidsField;

// Copyright (c) 2024 Rick Rutt

{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6018 off : unreachable code}
interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Dialogs, FGL, Graphics, LCLType,
  Boid,
  PascalBoidsRuleForm;

const
  DEFAULT_BOID_COUNT = 100;

type
  TPascalBoidsField = class(TCustomControl)
    public
      RuleForm: TPascalBoidsRuleForm;

      CurrentBoidCount: Integer;
      CurrentHawkCount: Integer;

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
    Boids: specialize TFPGObjectList<TBoid>;

  procedure TPascalBoidsField.Initialize;
  begin
    Boids := specialize TFPGObjectList<TBoid>.Create;

    CurrentBoidCount := 0;
    CurrentHawkCount := 0;

    OnMouseDown := @MouseDown;
  end;

  procedure TPascalBoidsField.Randomize(const BoidCount: Integer);
  var
    randomX: Integer;
    randomY: Integer;
    i: Integer;
    makeHawk: Boolean;
  begin
    if (CurrentBoidCount > 0) then begin
      Boids.Destroy;

      CurrentBoidCount := 0;
      CurrentHawkCount := 0;
    end;

    Boids := specialize TFPGObjectList<TBoid>.Create;

    for i := 1 to BoidCount do begin
      randomX := Trunc(1.0 + Random(Width - 1));
      randomY := Trunc(1.0 + Random(Height - 1));

      makeHawk := false;
      AddNewBoid(randomX, randomY, makeHawk);
    end;

    CurrentBoidCount := BoidCount;
    CurrentHawkCount := 0;
  end;

  procedure TPascalBoidsField.Iterate;
  var
    bi: TBoid;
    avoidHawk: Boolean;
    boidsEnumerator: specialize TFpGListEnumerator<TBoid>;
  begin
    boidsEnumerator := boids.GetEnumerator;
    while (boidsEnumerator.MoveNext) do begin
      bi := BoidsEnumerator.Current;

      bi.Flock(Boids, RuleForm.FlockDistance, RuleForm.FlockPower);

      bi.Align(Boids, RuleForm.AlignDistance, RuleForm.AlignPower);

      avoidHawk := false;
      bi.Avoid(Boids, avoidHawk, RuleForm.AvoidBoidDistance, RuleForm.AvoidBoidPower);

      avoidHawk := true;
      bi.Avoid(Boids, avoidHawk, RuleForm.AvoidHawkDistance, RuleForm.AvoidHawkPower);

      bi.AdjustVelocity;
    end;

    boidsEnumerator := boids.GetEnumerator;
    while (boidsEnumerator.MoveNext) do begin
      bi := BoidsEnumerator.Current;
      bi.MoveForward(RuleForm.MinSpeed, RuleForm.MaxSpeed);

      if (RuleForm.WrapAroundEdges) then begin
        bi.WrapAround(Width, Height);
      end else begin
        bi.BounceAwayFromWalls(Width, Height, RuleForm.BounceDistance);
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
    boidsEnumerator: specialize TFpGListEnumerator<TBoid>;
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

      boidsEnumerator := boids.GetEnumerator;
      while (boidsEnumerator.MoveNext) do begin
        bi := BoidsEnumerator.Current;
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
    bi := TBoid.Create;
    bi.Initialize(X, Y);
    Boids.Add(bi);
    Inc(CurrentBoidCount);
    i := CurrentBoidCount;
    bi.BoidNumber := i;
    bi.IsHawk := MakeHawk;

    if (MakeHawk) then begin
      Inc(CurrentHawkCount);
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
