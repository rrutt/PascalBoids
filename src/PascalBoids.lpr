program PascalBoids;

// Copyright (c) 2024 Rick Rutt

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  PascalBoidsMainForm,
  PascalBoidsRuleForm,
  Boid;

begin
  Randomize;

  RequireDerivedFormResource := True;
  Application.Initialize;

  // Create RuleForm first so that MainForm can access its fields and methods.
  Application.CreateForm(TPascalBoidsRuleForm, RuleForm);
  Application.CreateForm(TPascalBoidsMainForm, MainForm);

  Application.Run;
end.

