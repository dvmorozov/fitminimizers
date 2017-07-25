{------------------------------------------------------------------------------
    This software is distributed under GPL in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Copyright (C) D.Morozov (dvmorozov@hotmail.com)
------------------------------------------------------------------------------}
unit CBRCComponent;

{$IFDEF Lazarus}
{$MODE Delphi}
{$ENDIF}

interface

uses Classes;

type
	//	The object is freed only when reference counter is equal to zero.
	//	The object isn't freed automatically, the destructor must be called as usual.
	//	In the case of cyclical references objects will be undestructable.
    TCBRCComponent = class(TComponent)
	{  the component Controlled By References Counter (CBRC) }
    protected
        FRefCount: LongInt;         //  Reference counter.
        IntControlled: Boolean;     

        function _AddRef: Integer; virtual; stdcall; 
        function _Release: Integer; virtual; stdcall;
    public
        procedure Free;
        property RefCount: LongInt read FRefCount;
    end;

implementation

procedure TCBRCComponent.Free;
begin
    if Assigned(Self) then
    begin
        if RefCount = 0 then Destroy
        else IntControlled := True;
    end;
end;

function TCBRCComponent._AddRef: Integer; stdcall;
begin
    Inc(FRefCount);
    Result := RefCount;
end;

function TCBRCComponent._Release: Integer; stdcall;
begin
    Dec(FRefCount);
    Result := RefCount;
    if IntControlled and (Result = 0) then Free;
end;

initialization
    RegisterClass(TCBRCComponent);
end.

