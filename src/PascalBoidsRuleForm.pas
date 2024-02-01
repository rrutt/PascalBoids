unit PascalBoidsRuleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TPascalBoidsRuleForm }

  TPascalBoidsRuleForm = class(TForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);

  private

  public
    BounceOffWalls: Boolean;

    procedure FormCreate(Sender: TObject);
    procedure SetDefaultValues;
  end;

var
  RuleForm: TPascalBoidsRuleForm;


implementation

procedure TPascalBoidsRuleForm.SetDefaultValues;
begin
  BounceOffWalls := true;
end;

procedure TPascalBoidsRuleForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := false;
  ShowMessage('Please use the Hide button instead of closing this form.');
end;

procedure TPascalBoidsRuleForm.FormCreate(Sender: TObject);
begin
end;

{$R *.lfm}

end.

