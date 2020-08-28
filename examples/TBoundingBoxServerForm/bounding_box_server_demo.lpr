program bounding_box_server_demo;

{$define UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    bounding_box_server_form, optimizing_app;

{$R *.res}

begin
    RequireDerivedFormResource := True;

    Application.Initialize;
    Application.CreateForm(TBoundingBoxServerForm, BoundingBoxServerForm);
    { Must be after form creation. }
    OptimizingApp := TOptimizingApp.Create(nil);
    Application.Run;
end.
