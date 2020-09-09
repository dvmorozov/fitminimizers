program bounding_box_server_demo;

uses
  Vcl.Forms,
  bounding_box_form in 'bounding_box_form.pas' {Form1},
  bounding_box_server in '..\Common\bounding_box_server.pas',
  int_user_interaction in '..\Common\int_user_interaction.pas',
  Math3d in '..\Common\Math3d.pas',
  optimizing_app in '..\Common\optimizing_app.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBoundingBoxForm, BoundingBoxForm);
  OptimizingApp := TOptimizingApp.Create(IUserInteraction(BoundingBoxForm));
  Application.Run;
end.
