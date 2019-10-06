program bounding_box_server_demo;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    bounding_box_server_form { you can add units after this };

{$R *.res}

begin
    Application.Initialize;
    Application.CreateForm(TBoundingBoxServerForm, BoundingBoxServerForm);
    Application.Run;
end.
