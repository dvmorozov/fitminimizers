program bounding_box_server_demo;

uses
  Vcl.Forms,
  bounding_box_component_form in 'bounding_box_component_form.pas' {Form1},
  downhill_simplex_handler in 'downhill_simplex_handler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBoundingBoxComponentForm, BoundingBoxComponentForm);
  Application.Run;
end.
