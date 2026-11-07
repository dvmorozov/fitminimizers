program bounding_box_cli;

uses Forms, Interfaces, bounding_box_console, optimizing_app,
    int_user_interaction;

var
    Application: TBoundingBoxConsole;
begin
    Application := TBoundingBoxConsole.Create(nil);
    Application.Title:='Bounding Box CLI Demo';

    OptimizingApp := TOptimizingApp.Create(IUserInteraction(Application));

    Application.Run;
    Application.Free;
end.
