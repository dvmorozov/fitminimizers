{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

    Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                        LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                        Facebook: https://www.facebook.com/dmitry.v.morozov
------------------------------------------------------------------------------------------------------------------------}
unit Tools;

interface

uses SysUtils, Classes
{$IF NOT DEFINED(FPC)}
    , Windows
{$ENDIF};

procedure UtilizeObject(PtrToObject: TObject);

implementation

procedure UtilizeObject(PtrToObject: TObject);
begin
    PtrToObject.Free;
end;

end.
