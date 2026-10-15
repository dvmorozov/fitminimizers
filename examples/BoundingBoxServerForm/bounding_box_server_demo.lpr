program bounding_box_server_demo;

{$define UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms,
    bounding_box_form, optimizing_app, int_user_interaction;

{$R *.res}

begin
    RequireDerivedFormResource := True;

    Application.Initialize;
    Application.CreateForm(TBoundingBoxForm, BoundingBoxForm);

    OptimizingApp := TOptimizingApp.Create(IUserInteraction(BoundingBoxForm));
    Application.Run;
end.
