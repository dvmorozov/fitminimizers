program BoundingBoxConsole;

uses Forms, Interfaces, bounding_box_console;

var
    Application: TBoundingBoxConsole;
begin
    Application := TBoundingBoxConsole.Create(nil);
    Application.Title := 'Bounding Box Console Demo';
    Application.Run;
    Application.Free;
end.
