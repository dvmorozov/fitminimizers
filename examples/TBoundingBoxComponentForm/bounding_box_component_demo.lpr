program bounding_box_component_demo;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
    cthreads, {$ENDIF} {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, bounding_box_component_form, optimizing_app;

{$R *.res}

begin
    RequireDerivedFormResource := True;
    OptimizingApp := TOptimizingApp.Create;

    Application.Initialize;
    Application.CreateForm(TBoundingBoxComponentForm, BoundingBoxComponentForm);
    Application.Run;
end.
