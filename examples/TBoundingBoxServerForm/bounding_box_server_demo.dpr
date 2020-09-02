program bounding_box_server_demo;

uses
  Vcl.Forms,
  bounding_box_form in 'bounding_box_form.pas' {Form1},
  bounding_box_server in 'bounding_box_server.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TBoundingBoxForm, BoundingBoxServerForm);
  Application.Run;
end.
