unit PascalBoidsRuleForm;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin;

type

  { TPascalBoidsRuleForm }

  TPascalBoidsRuleForm = class(TForm)
    ButtonApply: TButton;
    ButtonSetDefaults: TButton;
    ButtonHide: TButton;
    EditAvoidHawkPower: TEdit;
    EditFlockPower: TEdit;
    EditAlignPower: TEdit;
    EditAvoidBoidPower: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    RadioButtonWrap: TRadioButton;
    RadioButtonBounce: TRadioButton;
    RadioGroupEdge: TRadioGroup;
    SpinEditAvoidBoidDistance: TSpinEdit;
    SpinEditAvoidHawkDistance: TSpinEdit;
    SpinEditFlockDistance: TSpinEdit;
    SpinEditAlignDistance: TSpinEdit;
    SpinEditBounceDistance: TSpinEdit;
    SpinEditMinSpeed: TSpinEdit;
    SpinEditMaxSpeed: TSpinEdit;
    procedure ButtonApplyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonHideMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonSetDefaultsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);

  private

  public
    FlockDistance: Integer;
    FlockPower: Single;

    AlignDistance: Integer;
    AlignPower: Single;

    AvoidBoidDistance: Integer;
    AvoidBoidPower: Single;

    AvoidHawkDistance: Integer;
    AvoidHawkPower: Single;

    MinSpeed: Integer;
    MaxSpeed: Integer;

    BounceDistance: Integer;

    BounceOffWalls: Boolean;

    procedure FormCreate(Sender: TObject);
    procedure SetDefaultValues;
    procedure CopyFieldsToControls;
    procedure CopyControlsToFields;
  end;

var
  RuleForm: TPascalBoidsRuleForm;


implementation

procedure TPascalBoidsRuleForm.SetDefaultValues;
begin
  FlockDistance := 50;
  FlockPower := 0.0003;

  AlignDistance := 50;
  AlignPower := 0.01;

  AvoidBoidDistance := 20;
  AvoidBoidPower := 0.0001;

  AvoidHawkDistance := 150;
  AvoidHawkPower := 0.00005;

  MinSpeed := 1;
  MaxSpeed := 5;

  BounceDistance := 20;

  BounceOffWalls := true;
end;

procedure TPascalBoidsRuleForm.CopyFieldsToControls;
begin
  SpinEditFlockDistance.Value := FlockDistance;
  EditFlockPower.Text := FormatFLoat('0.00000', FlockPower);

  SpinEditAlignDistance.Value := AlignDistance;
  EditAlignPower.Text := FormatFLoat('0.00000', AlignPower);

  SpinEditAvoidBoidDistance.Value := AvoidBoidDistance;
  EditAvoidBoidPower.Text := FormatFLoat('0.00000', AvoidBoidPower);

  SpinEditAvoidHawkDistance.Value := AvoidHawkDistance;
  EditAvoidHawkPower.Text := FormatFLoat('0.00000', AvoidHawkPower);

  SpinEditMinSpeed.Value := MaxSpeed;
  SpinEditMaxSpeed.Value := MaxSpeed;

  SpinEditBounceDistance.Value := BounceDistance;

  if (BounceOffWalls) then begin
    RadioButtonBounce.Checked := true;
  end else begin
    RadioButtonWrap.Checked := true;
  end;
end;

procedure TPascalBoidsRuleForm.CopyControlsToFields;
var
  ok: Boolean;
  value: Single;
begin
  FlockDistance := SpinEditFlockDistance.Value;
  ok := TryStrToFloat(EditFlockPower.Text, value);
  if (ok) then begin
    FlockPower := value;
  end else begin
    EditFlockPower.Text := FormatFLoat('0.00000', FlockPower);
  end;

  AlignDistance := SpinEditAlignDistance.Value;
  ok := TryStrToFloat(EditAlignPower.Text, value);
  if (ok) then begin
    AlignPower := value;
  end else begin
    EditAlignPower.Text := FormatFLoat('0.00000', AlignPower);
  end;

  AvoidBoidDistance := SpinEditAvoidBoidDistance.Value;
  ok := TryStrToFloat(EditAvoidBoidPower.Text, value);
  if (ok) then begin
    AvoidBoidPower := value;
  end else begin
    EditAvoidBoidPower.Text := FormatFLoat('0.00000', AvoidBoidPower);
  end;

  AvoidHawkDistance := SpinEditAvoidHawkDistance.Value;
  ok := TryStrToFloat(EditAvoidHawkPower.Text, value);
  if (ok) then begin
    AvoidHawkPower := value;
  end else begin
    EditAvoidHawkPower.Text := FormatFLoat('0.00000', AvoidHawkPower);
  end;

  MinSpeed := SpinEditMinSpeed.Value;
  MaxSpeed := SpinEditMaxSpeed.Value;

  BounceDistance := SpinEditBounceDistance.Value;

  if (RadioGroupEdge.ItemIndex = 0) then begin
    BounceOffWalls := true;
  end else begin
    BounceOffWalls := false;
  end;
end;

procedure TPascalBoidsRuleForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := false;
  ShowMessage('Please use the Hide button instead of closing this form.');
end;

procedure TPascalBoidsRuleForm.ButtonSetDefaultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetDefaultValues;
  CopyFieldsToControls;
end;

procedure TPascalBoidsRuleForm.ButtonHideMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Hide;
end;

procedure TPascalBoidsRuleForm.ButtonApplyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CopyControlsToFields;
end;

procedure TPascalBoidsRuleForm.FormCreate(Sender: TObject);
begin
end;

{$R *.lfm}

end.

