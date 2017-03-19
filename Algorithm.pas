{------------------------------------------------------------------------------
    This software is distributed under GPL in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) D.Morozov (dvmorozov@hotmail.com)
------------------------------------------------------------------------------}
unit Algorithm;

{$MODE Delphi}

interface

uses Classes;

type
    TAlgorithm = class(TComponent)
    public
        procedure AlgorithmRealization; virtual; abstract;
    end;


implementation

initialization
    RegisterClass(TAlgorithm);
end.


