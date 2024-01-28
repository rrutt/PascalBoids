unit PascalBoidsField;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Dialogs, Graphics, LCLType, Math,
  Boid;

const
  MAXIMUM_BOID_COUNT = 10000;
  DEFAULT_BOID_COUNT = 10;
  ZOOM_DIVISOR = 120;

type
  TPascalBoidsField = class(TCustomControl)
    private
      InitialBoidCount: Integer;
      CenterX: Integer;
      CenterY: Integer;
      ViewOffsetX: Integer;
      ViewOffsetY: Integer;
      ZoomFactor: Integer;
      InverseZoom: Single;

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
      Procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
        WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
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

    ViewOffsetX := 0;
    ViewOffsetY := 0;

    ZoomFactor := 1;
    InverseZoom := 1.0 / ZoomFactor;

    for i := 1 to MAXIMUM_BOID_COUNT do begin
      a := TBoid.Create;
      Boids[i] := a;
    end;

    ActiveBoidCount := 0;

    OnMouseDown := @MouseDown;
    OnMouseWheel := @MouseWheel;
  end;

  procedure TPascalBoidsField.Randomize(const BoidCount: Integer);
  var
    i: Integer;
  begin
    InitialBoidCount := BoidCount;

    ViewOffsetX := CenterX;
    ViewOffsetY := CenterY;

    for i := 1 to InitialBoidCount do begin
      Boids[i].Randomize(Width, Height);
    end;

    ActiveBoidCount := InitialBoidCount;
  end;

  procedure TPascalBoidsField.Iterate;
  var
    i: Integer;
    j: Integer;
    ai: TBoid;
    aj: TBoid;
  begin
    for i := 1 to InitialBoidCount do begin
      ai := Boids[i];
      if (ai.IsActive) then begin
        for j := 1 to InitialBoidCount do begin
          aj := Boids[j];
          if ((i <> j) and aj.IsActive) then begin
            if (ai.MergeIfAdjacent(aj)) then begin
              Dec(ActiveBoidCount);
            end;
          end;
        end;
      end;
    end;

    for i := 1 to InitialBoidCount do begin
      ai := Boids[i];
      if (ai.IsActive) then begin
        ai.AccelerationX := 0.0;
        ai.AccelerationY := 0.0;

        for j := 1 to InitialBoidCount do begin
          aj := Boids[j];
          if ((i <> j) and aj.IsActive) then begin
            ai.Accelerate(aj);
          end;
        end;
      end;
    end;

    for i := 1 to InitialBoidCount do begin
      ai := Boids[i];
      if (ai.IsActive) then begin
        ai.Move;
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

      Bitmap.Canvas.Pen.Color := clWhite;
      Bitmap.Canvas.Brush.Color := clWhite;
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
    offsetX: Integer;
    offsetY: Integer;
    newX: Integer;
    newY: Integer;
  begin
    offsetX := CenterX - X;
    offsetY := CenterY - Y;

    if (Button = mbLeft) then begin
      { Re-center the space on the clicked point. }
      ViewOffsetX := ViewOffsetX + offsetX;
      ViewOffsetY := ViewOffsetY + offsetY;
    end else if (Button = mbRight) then begin
      newX := Trunc((X - ViewOffsetX) * ZoomFactor);
      newY := Trunc((Y - ViewOffsetY) * ZoomFactor);

      AddNewBoid(newX, newY);
    end;

    Paint;
  end;

  Procedure TPascalBoidsField.MouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    ZoomFactor := Max(ZoomFactor - (WheelDelta div ZOOM_DIVISOR), 1);
    InverseZoom := 1.0 / ZoomFactor;

    Paint;
  end;

begin
end.
