unit PascalBoidsMainForm;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  PascalBoidsRuleForm,
  PascalBoidsField;

const
  PRODUCT_VERSION = '1.0.0+20240131';
  MAXIMUM_BOID_COUNT = 1000;

type

  { TPascalBoidsMainForm }

  TPascalBoidsMainForm = class(TForm)
    ButtonStep: TButton;
    ButtonPause: TButton;
    ButtonStart: TButton;
    ButtonRandomize: TButton;
    ButtonRules: TButton;
    Label1: TLabel;
    Label3: TLabel;
    LabelBoidCount: TLabel;
    LabelBoidCount1: TLabel;
    Label2: TLabel;
    SpinEditBoidCount: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRandomizeClick(Sender: TObject);
    procedure ButtonRulesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStepClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ResizeField;
  end;

  var
    MainForm: TPascalBoidsMainForm;

implementation

{$R *.lfm}

  var
   Field: TPascalBoidsField;
   //ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

procedure TPascalBoidsMainForm.FormCreate(Sender: TObject);
begin
  Caption := Caption + '  (Version ' + PRODUCT_VERSION + ')';

  Timer1.Interval := 10; // milliseconds
  Timer1.Enabled := false;

  SpinEditBoidCount.MinValue := 1;
  SpinEditBoidCount.Increment := 5;
  SpinEditBoidCount.MaxValue := MAXIMUM_BOID_COUNT;
  SpinEditBoidCount.Value := DEFAULT_BOID_COUNT;

  RuleForm.SetDefaultValues;

  Field := TPascalBoidsField.Create(Self);
  ResizeField;

  LabelBoidCount.Caption := Format('%d', [Field.CurrentBoidCount]);

  ButtonStart.Enabled := false;
end;

procedure TPascalBoidsMainForm.ResizeField;
const
  BORDER_SIZE = 10;
begin
  Field.Top := ButtonRandomize.Top + ButtonRandomize.Height + BORDER_SIZE;
  Field.Left := BORDER_SIZE;
  Field.Width := Self.Width - (2 * BORDER_SIZE);
  Field.Height := Self.Height - (ButtonRandomize.Top + ButtonRandomize.Height + (2 * BORDER_SIZE));
  Field.Parent := Self;
  Field.Initialize;
  Field.DoubleBuffered := True;

  Field.Paint;
end;

procedure TPascalBoidsMainForm.Timer1Timer(Sender: TObject);
begin
  // Disable the timer to avoid double fire during extended iteration processing.
  Timer1.Enabled := false;

  Field.Iterate;
  Field.Paint;

  LabelBoidCount.Caption := Format('%d', [Field.CurrentBoidCount]);

  Timer1.Enabled := true;
end;

procedure TPascalBoidsMainForm.ButtonRandomizeClick(Sender: TObject);
begin
  ButtonRandomize.Enabled := false;

  ResizeField;

  Field.Randomize(SpinEditBoidCount.Value);
  Field.Paint;

  LabelBoidCount.Caption := Format('%d', [Field.CurrentBoidCount]);

  ButtonRandomize.Enabled := true;
  ButtonStart.Enabled := true;
  ButtonStep.Enabled := true;
  ButtonRules.Enabled := true;
end;

procedure TPascalBoidsMainForm.ButtonRulesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RuleForm.Show;
end;

procedure TPascalBoidsMainForm.ButtonPauseClick(Sender: TObject);
begin
  ButtonPause.Enabled := false;

  Timer1.Enabled := false;

  ButtonRandomize.Enabled := true;
  ButtonStart.Enabled := true;
  ButtonStep.Enabled := true;
  ButtonRules.Enabled := true;
end;

procedure TPascalBoidsMainForm.ButtonStartClick(Sender: TObject);
begin
  ButtonStart.Enabled := false;
  ButtonRandomize.Enabled := false;
  ButtonStep.Enabled := false;

  Timer1.Enabled := true;

  ButtonPause.Enabled := true;
  ButtonRules.Enabled := false;

  RuleForm.Hide;
end;

procedure TPascalBoidsMainForm.ButtonStepClick(Sender: TObject);
begin
  Field.Iterate;
  Field.Paint;

  LabelBoidCount.Caption := Format('%d', [Field.CurrentBoidCount]);
end;

procedure TPascalBoidsMainForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FreeAndNil(RuleForm);
  Application.Terminate;
end;

end.

