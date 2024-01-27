program PascalBoids;

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
  Boid;

begin
  Randomize;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TPascalBoidsMainForm, MainForm);
  Application.Run;
end.

