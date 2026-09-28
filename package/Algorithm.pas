{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

    Copyright (C) Dmitry Morozov
------------------------------------------------------------------------------------------------------------------------}
unit Algorithm;

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
