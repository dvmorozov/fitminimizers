{------------------------------------------------------------------------------
    This software is distributed under GPL in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) D.Morozov (dvmorozov@hotmail.com)
------------------------------------------------------------------------------}
unit MyExceptions;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses
    Classes, SysUtils;

type
    //  исключение выбрасывается в случае
    //  неправильных действий пользователя
    EUserException = class(Exception);

implementation

initialization
end.

