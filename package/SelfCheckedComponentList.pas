{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

    Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                        LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                        Facebook: https://www.facebook.com/dmitry.v.morozov
------------------------------------------------------------------------------------------------------------------------}
unit SelfCheckedComponentList;

interface

uses
{$IFDEF Lazarus}
    LCLIntf,
{$ELSE}
    { This causes warning when compiled under Lazarus. }
    Types,
{$ENDIF}
    SysUtils, Classes, CBRCComponent, Tools;

type
    ISelfChecked = interface
        ['{E7E7008A-EE1C-4828-B1D6-A53806820A66}']
        procedure IsReady;
        function MyNameIs: string;
    end;

const
    SelfCheckedGUID: TGUID = '{E7E7008A-EE1C-4828-B1D6-A53806820A66}';

type
    { By default release stored components. }
    TSelfCheckedComponentList = class(TCBRCComponent, ISelfChecked)
    protected
        List: TList;
        State: LongInt;

        function GetCount: Integer;
        function GetItem(index: Integer): TComponent;
        function GetCapacity: Integer;

        procedure SetItem(index: Integer; Item: TComponent);
        procedure SetCapacity(ACapacity: Integer);

        procedure ReadList(Reader: TReader);
        procedure WriteList(Writer: TWriter);

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure DefineProperties(Filer: TFiler); override;

        procedure IsReady; virtual;
        function MyNameIs: string; virtual;

        procedure Sort(Compare: TListSortCompare);
        procedure Pack;
        function GetState: LongInt;
        procedure SetState(AState: LongInt);

        procedure Clear;
        procedure ClearAll;
        function Add(Item: TComponent): Integer; virtual;
        procedure Delete(Index: Integer); virtual;
        procedure Insert(Index: Integer; Item: TComponent); virtual;
        function Extract(Item: Pointer): Pointer;
        function Remove(Item: Pointer): Integer;
        function IndexOf(Item: Pointer): Integer;

        property Items[index: Integer]: TComponent read GetItem write SetItem;
        property Capacity: Integer read GetCapacity write SetCapacity;
        property Count: Integer read GetCount;
    end;

const
    cfActive: LongInt = 1;
    cfPassive: LongInt = 2;

type
    TSelfCleanList = class(TList)
    public
        procedure ClearAll; virtual;
    end;

procedure Register;

implementation

procedure Register;
begin
end;

constructor TSelfCheckedComponentList.Create;
begin
    inherited Create(AOwner);
    List := TList.Create;
    SetState(cfActive);
end;

destructor TSelfCheckedComponentList.Destroy;
begin
    Clear;
    UtilizeObject(List);
    inherited Destroy;
end;

procedure TSelfCheckedComponentList.DefineProperties(Filer: TFiler);
begin
    Filer.DefineProperty('List', ReadList, WriteList, True);
end;

procedure TSelfCheckedComponentList.ReadList(Reader: TReader);
var
    i, CompCount: LongInt;
begin
    CompCount := Reader.ReadInteger;
    for i := 1 to CompCount do
        Add(Reader.ReadComponent(nil));
end;

procedure TSelfCheckedComponentList.WriteList(Writer: TWriter);
var
    i: LongInt;
begin
    Writer.WriteInteger(Count);
    for i := 0 to Count - 1 do
        Writer.WriteComponent(Items[i]);
end;

procedure TSelfCheckedComponentList.Clear;
begin
    if State = cfActive then
        ClearAll;
    List.Clear;
end;

function TSelfCheckedComponentList.GetCount;
begin
    GetCount := List.Count;
end;

function TSelfCheckedComponentList.GetItem;
begin
    Result := List.Items[Index];
end;

procedure TSelfCheckedComponentList.SetItem;
begin
    List.Items[Index] := Item;
end;

function TSelfCheckedComponentList.GetCapacity: Integer;
begin
    GetCapacity := List.Capacity;
end;

procedure TSelfCheckedComponentList.SetCapacity(ACapacity: Integer);
begin
    List.Capacity := ACapacity;
end;

function TSelfCheckedComponentList.Add;
begin
    Add := List.Add(Item);
end;

procedure TSelfCheckedComponentList.Sort(Compare: TListSortCompare);
begin
    List.Sort(Compare);
end;

procedure TSelfCheckedComponentList.Delete(Index: Integer);
var
    TC: TComponent;
begin
    if State = cfActive then
    begin
        TC := Items[Index];
        UtilizeObject(TC);
    end;
    List.Delete(Index);
end;

function TSelfCheckedComponentList.Extract(Item: Pointer): Pointer;
begin
    Result := List.Extract(Item);
end;

function TSelfCheckedComponentList.Remove(Item: Pointer): Integer;
begin
    Result := IndexOf(Item);
    Delete(Result);
end;

procedure TSelfCheckedComponentList.ClearAll;
begin
    while Count <> 0 do
        Delete(0);
end;

function TSelfCheckedComponentList.IndexOf(Item: Pointer): Integer;
begin
    Result := List.IndexOf(Item);
end;

procedure TSelfCheckedComponentList.SetState;
begin
    State := AState;
end;

function TSelfCheckedComponentList.GetState: LongInt;
begin
    Result := State;
end;

procedure TSelfCheckedComponentList.Insert(Index: Integer; Item: TComponent);
begin
    List.Insert(Index, Item);
end;

procedure TSelfCheckedComponentList.Pack;
begin
    List.Pack;
end;

procedure TSelfCleanList.ClearAll;
var
    i: LongInt;
    Item: Pointer;
begin
    for i := 0 to Count - 1 do
    begin
        Item := Items[i];
        if Assigned(Item) then
            with TObject(Item) do
                try
                    UtilizeObject(TObject(Item));
                    Items[i] := nil;
                except
                    Items[i] := nil
                end;
    end;
    Clear;
end;

procedure TSelfCheckedComponentList.IsReady;
var
    i: LongInt;
    ISC: ISelfChecked;
begin
    for i := 0 to Count - 1 do
        if Items[i].GetInterface(SelfCheckedGUID, ISC) then
            ISC.IsReady;
end;

function TSelfCheckedComponentList.MyNameIs: string;
begin
    Result := '';
end;

initialization
    RegisterClass(TSelfCheckedComponentList);
end.
