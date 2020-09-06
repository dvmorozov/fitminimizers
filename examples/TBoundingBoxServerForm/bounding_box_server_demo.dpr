program bounding_box_server_demo;

uses
  Vcl.Forms,
  bounding_box_form in 'bounding_box_form.pas' {Form1},
  bounding_box_server in 'bounding_box_server.pas',
  optimizing_app in '..\optimizing_app.pas',
  int_user_interaction in '..\int_user_interaction.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBoundingBoxForm, BoundingBoxForm);
  OptimizingApp := TOptimizingApp.Create(nil);
  Application.Run;
end.
