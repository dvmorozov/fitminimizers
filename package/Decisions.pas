{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

    Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                        LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                        Facebook: https://www.facebook.com/dmitry.v.morozov
------------------------------------------------------------------------------------------------------------------------}
unit Decisions;

interface

uses SysUtils, Classes, Contnrs;

const
    TINY = 1e-6;

type
    TAbstractDecision = class(TComponent)
    protected
        { Value can be negative! }
        FEvaluation: Double;
        function GetParametersNumber: LongInt; virtual; abstract;
        procedure SetParametersNumber(AParametersNumber: LongInt); virtual; abstract;
    public
        { Initializes all the fields of created copy of solution. }
        procedure FillCopy(const Copy: TAbstractDecision); virtual; abstract;
        function GetCopy: TAbstractDecision; virtual; abstract;
        { Returns True if all values of parameters of solutions coincide. }
        function Coincide(const Decision: TAbstractDecision): Boolean; virtual; abstract;

        property Evaluation: Double read FEvaluation write FEvaluation;
        property ParametersNumber: LongInt read GetParametersNumber
            write SetParametersNumber;
    end;

    TOneDimDecision = class(TAbstractDecision)
    public
        { Inverts parameter to opposite value (negates). }
        procedure InvertParameter(const ParamNum: LongInt); virtual; abstract;
        { Swaps parameters of solution with given indicies. }
        procedure ExchangeParameters(const ParamNum1, ParamNum2: LongInt);
            virtual; abstract;
        { Exchanges parameters with another solution. }
        procedure ExchangeWithOuter(const Decision: TAbstractDecision;
            ParamNum: LongInt); virtual; abstract;
        { Copies value of parameter into new position. }
        procedure CopyParameter(const ParamNum, NewParamNum: LongInt);
            virtual; abstract;
    end;

    { Abstract component for building solutions with two-dimensional array of parameters.
      Every row of the array represents single gene. }
    TTwoDimDecision = class(TAbstractDecision)
    protected
        function GetGenesNumber: LongInt; virtual; abstract;
        procedure SetGenesNumber(AGenesNumber: LongInt); virtual; abstract;
    public
        { Inverts parameter to opposite value (negates). }
        procedure InvertParameter(const GeneNum, ParamNum: LongInt);
            virtual; abstract;
        procedure InvertBlock(
            const StartGeneNum, EndGeneNum, StartParamNum, EndParamNum: LongInt); virtual;
        { Swaps parameters of the solution. }
        procedure ExchangeParameters(const GeneNum1,
            ParamNum1, GeneNum2, ParamNum2: LongInt); virtual; abstract;
        { Exchanges parameters with another solution. }
        procedure ExchangeWithOuter(const Decision: TAbstractDecision;
            MyGeneNum, OuterGeneNum, ParamNum: LongInt); virtual; abstract;
        procedure ExchangeBlocksWithOuter(const Decision: TAbstractDecision;
            StartGeneNum, EndGeneNum, StartParamNum, EndParamNum: LongInt); virtual;
        { Copies values of parameters into given positions. }
        procedure CopyParameter(
            const SrcGeneNum, SrcParamNum, DestGeneNum, DestParamNum: LongInt);
            virtual; abstract;
        procedure CopyBlock(const StartGeneNum, EndGeneNum, StartParamNum,
            EndParamNum, GeneOffset, ParamOffset: LongInt); virtual; abstract;

        property GenesNumber: LongInt read GetGenesNumber write SetGenesNumber;
    end;

    { Base classes to work with different types of solutions. }

    EFloatDecision = class(Exception);

    TFloatDecision = class(TOneDimDecision)
    protected
        FParameters: array of Double;
        function GetParameter(index: LongInt): Double;
        procedure SetParameter(index: LongInt; AParameter: Double);
        function GetParametersNumber: LongInt; override;
        procedure SetParametersNumber(AParametersNumber: LongInt); override;
    public
        destructor Destroy; override;
        procedure FillCopy(const Copy: TAbstractDecision); override;
        function GetCopy: TAbstractDecision; override;
        function Coincide(const Decision: TAbstractDecision): Boolean; override;
        procedure InvertParameter(const ParamNum: LongInt); override;
        procedure ExchangeParameters(const ParamNum1, ParamNum2: LongInt); override;
        procedure ExchangeWithOuter(const Decision: TAbstractDecision;
            ParamNum: LongInt); override;
        procedure CopyParameter(const ParamNum, NewParamNum: LongInt); override;
        { Index is zero-based. }
        property Parameters[index: LongInt]: Double read GetParameter write SetParameter;
            default;
    end;

    EByteDecision = class(Exception);

    TByteDecision = class(TOneDimDecision)
    protected
        FParameters: array of Byte;
        function GetParameter(index: LongInt): Byte;
        procedure SetParameter(index: LongInt; AParameter: Byte);
        function GetParametersNumber: LongInt; override;
        procedure SetParametersNumber(AParametersNumber: LongInt); override;
    public
        destructor Destroy; override;
        procedure FillCopy(const Copy: TAbstractDecision); override;
        function GetCopy: TAbstractDecision; override;
        function Coincide(const Decision: TAbstractDecision): Boolean; override;
        { Inverts byte by means of NOT operation. }
        procedure InvertParameter(const ParamNum: LongInt); override;
        procedure ExchangeParameters(const ParamNum1, ParamNum2: LongInt); override;
        procedure ExchangeWithOuter(const Decision: TAbstractDecision;
            ParamNum: LongInt); override;
        procedure CopyParameter(const ParamNum, NewParamNum: LongInt); override;

        property Parameters[index: LongInt]: Byte read GetParameter write SetParameter;
            default;
    end;

    ETwoDimFloatDecision = class(Exception);

    { The "gene" corresponds to column of two-dimensional matrix. }
    TTwoDimFloatDecision = class(TTwoDimDecision)
    protected
        FParameters: array of array of Double;
        FParametersNumber: LongInt;
        FSelectedGene: LongInt;
        function GetParameter(index: LongInt): Double;
        procedure SetParameter(index: LongInt; AParameter: Double);
        function GetParametersNumber: LongInt; override;
        procedure SetParametersNumber(AParametersNumber: LongInt); override;
        function GetGenesNumber: LongInt; override;
        procedure SetGenesNumber(AGenesNumber: LongInt); override;
        function GetSelectedGene: LongInt;
        procedure SetSelectedGene(ASelectedGene: LongInt);
    public
        destructor Destroy; override;
        procedure FillCopy(const Copy: TAbstractDecision); override;
        function GetCopy: TAbstractDecision; override;
        function Coincide(const Decision: TAbstractDecision): Boolean; override;

        procedure InvertParameter(const GeneNum, ParamNum: LongInt); override;
        procedure ExchangeParameters(const GeneNum1,
            ParamNum1, GeneNum2, ParamNum2: LongInt); override;
        procedure CopyParameter(
            const SrcGeneNum, SrcParamNum, DestGeneNum, DestParamNum: LongInt); override;
        procedure ExchangeWithOuter(const Decision: TAbstractDecision;
            MyGeneNum, OuterGeneNum, ParamNum: LongInt); override;
        { EndGeneNum must be >= StartGeneNum, EndParamNum must be >= StartParamNum! }
        procedure CopyBlock(const StartGeneNum, EndGeneNum, StartParamNum,
            EndParamNum, GeneOffset, ParamOffset: LongInt); override;

        property Parameters[index: LongInt]: Double read GetParameter write SetParameter;
            default;
        property SelectedGene: LongInt read GetSelectedGene write SetSelectedGene;
    end;

    EDecisionsList = class(Exception);

    TDecisionsList = class(TComponentList)
    public
        { Returns solution having maximum estimation value less than UpLimit,
          starting from 'StartIndex'. Items must be sorted by decreasing of
          estimation value. }
        function GetMaxDecision(const StartIndex: LongInt;
            UpLimit: Double): TAbstractDecision;
        { Returns solution having minimum estimation value greater than LowLimit,
          starting from 'StartIndex'. Items must be sorted by increasing of
          estimation value. }
        function GetMinDecision(const StartIndex: LongInt;
            LowLimit: Double): TAbstractDecision;
        function GetAbsoluteMin: TAbstractDecision;
        function GetAbsoluteMax: TAbstractDecision;
        function HasThisDecision(const Decision: TAbstractDecision): Boolean;
    end;

{ Sorting by decreasing of estimation value. }
function EvalDownSortFunc(Item1, Item2: Pointer): Integer;
{ Sorting by increasing of estimation value. }
function EvalUpSortFunc(Item1, Item2: Pointer): Integer;

const
    EvalDownSort: TListSortCompare = EvalDownSortFunc;
    EvalUpSort: TListSortCompare = EvalUpSortFunc;

implementation

destructor TFloatDecision.Destroy;
begin
    Finalize(FParameters);
    inherited Destroy;
end;

function TFloatDecision.GetParameter(index: LongInt): Double;
begin
    if (index > ParametersNumber - 1) or (index < 0) then
        raise EFloatDecision.Create('Parameter index out of range...');
    Result := FParameters[index];
end;

procedure TFloatDecision.SetParameter(index: LongInt; AParameter: Double);
begin
    if (index > ParametersNumber - 1) or (index < 0) then
        raise EFloatDecision.Create('Parameter index out of range...');
    FParameters[index] := AParameter;
end;

function TFloatDecision.GetParametersNumber: LongInt;
begin
    if Assigned(FParameters) then
        Result := Length(FParameters)
    else
        Result := 0;
end;

procedure TFloatDecision.SetParametersNumber(AParametersNumber: LongInt);
var
    i: LongInt;
    SavedLength: LongInt;
begin
    if AParametersNumber > ParametersNumber then
    begin
        SavedLength := ParametersNumber;
        SetLength(FParameters, AParametersNumber);
        for i := SavedLength to ParametersNumber - 1 do
            Parameters[i] := 0;
    end
    else
        SetLength(FParameters, AParametersNumber);
end;

function TFloatDecision.GetCopy: TAbstractDecision;
begin
    Result := TFloatDecision.Create(nil);
    FillCopy(Result);
end;

procedure TFloatDecision.FillCopy(const Copy: TAbstractDecision);
var
    i: LongInt;
begin
    TFloatDecision(Copy).ParametersNumber := ParametersNumber;
    for i := 0 to ParametersNumber - 1 do
        TFloatDecision(Copy).Parameters[i] := Parameters[i];
    Copy.Evaluation := Evaluation;
end;

function TFloatDecision.Coincide(const Decision: TAbstractDecision): Boolean;
var
    FloatDecision: TFloatDecision absolute Decision;
    i: LongInt;
begin
    if not (Decision is TFloatDecision) then
        raise EFloatDecision.Create('Invalid decision type...');
    Result := True;
    for i := 0 to ParametersNumber - 1 do
    begin
        if Abs(FloatDecision[i] - Self[i]) >= TINY then
        begin
            Result := False;
            Exit;
        end;
    end;
end;

procedure TFloatDecision.InvertParameter(const ParamNum: LongInt);
begin
    Self[ParamNum] := (-1) * Self[ParamNum];
end;

procedure TFloatDecision.ExchangeParameters(const ParamNum1, ParamNum2: LongInt);
var
    TempDouble: Double;
begin
    TempDouble := Self[ParamNum2];
    Self[ParamNum2] := Self[ParamNum1];
    Self[ParamNum1] := TempDouble;
end;

procedure TFloatDecision.ExchangeWithOuter(const Decision: TAbstractDecision;
    ParamNum: LongInt);
var
    TempDouble: Double;
begin
    if not (Decision is TFloatDecision) then
        raise EFloatDecision.Create('Invalid decision type ' + Decision.ClassName);

    TempDouble := Self[ParamNum];
    Self[ParamNum] := TFloatDecision(Decision)[ParamNum];
    TFloatDecision(Decision)[ParamNum] := TempDouble;
end;

procedure TFloatDecision.CopyParameter(const ParamNum, NewParamNum: LongInt);
begin
    Self[NewParamNum] := Self[ParamNum];
end;

destructor TByteDecision.Destroy;
begin
    Finalize(FParameters);
    inherited Destroy;
end;

function TByteDecision.GetParameter(index: LongInt): Byte;
begin
    if (index > ParametersNumber - 1) or (index < 0) then
        raise EByteDecision.Create('Parameter index out of range...');
    Result := FParameters[index];
end;

procedure TByteDecision.SetParameter(index: LongInt; AParameter: Byte);
begin
    if (index > ParametersNumber - 1) or (index < 0) then
        raise EByteDecision.Create('Parameter index out of range...');
    FParameters[index] := AParameter;
end;

function TByteDecision.GetParametersNumber: LongInt;
begin
    if Assigned(FParameters) then
        Result := Length(FParameters)
    else
        Result := 0;
end;

procedure TByteDecision.SetParametersNumber(AParametersNumber: LongInt);
var
    i: LongInt;
    SavedLength: LongInt;
begin
    if AParametersNumber > ParametersNumber then
    begin
        SavedLength := ParametersNumber;
        SetLength(FParameters, AParametersNumber);
        for i := SavedLength to ParametersNumber - 1 do
            Parameters[i] := 0;
    end
    else
        SetLength(FParameters, AParametersNumber);
end;

function TByteDecision.GetCopy: TAbstractDecision;
begin
    Result := TByteDecision.Create(nil);
    FillCopy(Result);
end;

procedure TByteDecision.FillCopy(const Copy: TAbstractDecision);
var
    i: LongInt;
begin
    TByteDecision(Copy).ParametersNumber := ParametersNumber;
    for i := 0 to ParametersNumber - 1 do
        TByteDecision(Copy).Parameters[i] := Parameters[i];
    Copy.Evaluation := Evaluation;
end;

function TByteDecision.Coincide(const Decision: TAbstractDecision): Boolean;
var
    ByteDecision: TByteDecision absolute Decision;
var
    i: LongInt;
begin
    if not (Decision is TByteDecision) then
        raise EByteDecision.Create('Invalid decision type...');
    Result := True;
    for i := 0 to ParametersNumber - 1 do
    begin
        if ByteDecision[i] <> Self[i] then
        begin
            Result := False;
            Exit;
        end;
    end;
end;

procedure TByteDecision.InvertParameter(const ParamNum: LongInt);
begin
    Self[ParamNum] := not Self[ParamNum];
end;

procedure TByteDecision.ExchangeParameters(const ParamNum1, ParamNum2: LongInt);
var
    TempByte: Byte;
begin
    TempByte := Self[ParamNum2];
    Self[ParamNum2] := Self[ParamNum1];
    Self[ParamNum1] := TempByte;
end;

procedure TByteDecision.ExchangeWithOuter(const Decision: TAbstractDecision;
    ParamNum: LongInt);
var
    TempByte: Byte;
begin
    if not (Decision is TByteDecision) then
        raise EByteDecision.Create('Invalid decision type ' + Decision.ClassName);

    TempByte := Self[ParamNum];
    Self[ParamNum] := TByteDecision(Decision)[ParamNum];
    TFloatDecision(Decision)[ParamNum] := TempByte;
end;

procedure TByteDecision.CopyParameter(const ParamNum, NewParamNum: LongInt);
begin
    Self[NewParamNum] := Self[ParamNum];
end;

function TTwoDimFloatDecision.GetParameter(index: LongInt): Double;
begin
    if (index > ParametersNumber - 1) or (index < 0) then
        raise ETwoDimFloatDecision.Create('Parameter index out of range...');
    Result := FParameters[SelectedGene, index];
end;

procedure TTwoDimFloatDecision.SetParameter(index: LongInt; AParameter: Double);
begin
    if (index > ParametersNumber - 1) or (index < 0) then
        raise ETwoDimFloatDecision.Create('Parameter index out of range...');
    FParameters[SelectedGene, index] := AParameter;
end;

function TTwoDimFloatDecision.GetParametersNumber: LongInt;
begin
    Result := FParametersNumber;
end;

procedure TTwoDimFloatDecision.SetParametersNumber(AParametersNumber: LongInt);
var
    i: LongInt;
begin
    FParametersNumber := AParametersNumber;
    for i := 0 to Length(FParameters) - 1 do
        SetLength(FParameters[i], AParametersNumber);
end;

function TTwoDimFloatDecision.GetGenesNumber: LongInt;
begin
    if Assigned(FParameters) then
        Result := Length(FParameters)
    else
        Result := 0;
end;

procedure TTwoDimFloatDecision.SetGenesNumber(AGenesNumber: LongInt);
var
    SavedLength: LongInt;
    i: LongInt;
begin
    SavedLength := Length(FParameters);
    SetLength(FParameters, AGenesNumber);
    if Length(FParameters) > SavedLength then
        for i := SavedLength to Length(FParameters) - 1 do
            SetLength(FParameters[i], ParametersNumber);
end;

procedure TTwoDimFloatDecision.SetSelectedGene(ASelectedGene: LongInt);
begin
    if (ASelectedGene > GenesNumber - 1) or (ASelectedGene < 0) then
        raise ETwoDimFloatDecision.Create('Selected gene out of range...');
    FSelectedGene := ASelectedGene;
end;

function TTwoDimFloatDecision.GetSelectedGene: LongInt;
begin
    if (FSelectedGene > GenesNumber - 1) then
        raise ETwoDimFloatDecision.Create('Selected gene out of range...');
    Result := FSelectedGene;
end;

procedure TTwoDimFloatDecision.FillCopy(const Copy: TAbstractDecision);
var
    i, j: LongInt;
begin
    TTwoDimFloatDecision(Copy).GenesNumber := GenesNumber;
    TTwoDimFloatDecision(Copy).ParametersNumber := ParametersNumber;
    for j := 0 to GenesNumber - 1 do
    begin
        TTwoDimFloatDecision(Copy).SelectedGene := j;
        SelectedGene := j;
        for i := 0 to ParametersNumber - 1 do
            TTwoDimFloatDecision(Copy).Parameters[i] := Parameters[i];
    end;
    Copy.Evaluation := Evaluation;
end;

destructor TTwoDimFloatDecision.Destroy;
var
    i: LongInt;
begin
    for i := 0 to Length(FParameters) - 1 do
        Finalize(FParameters[i]);
    Finalize(FParameters);
    inherited Destroy;
end;

function TTwoDimFloatDecision.GetCopy: TAbstractDecision;
begin
    Result := TTwoDimFloatDecision.Create(nil);
    FillCopy(Result);
end;

function TTwoDimFloatDecision.Coincide(const Decision: TAbstractDecision): Boolean;
var
    i, j: LongInt;
    TwoDimFloatDecision: TTwoDimFloatDecision absolute Decision;
begin
    if not (Decision is TTwoDimFloatDecision) then
        raise ETwoDimFloatDecision.Create('Invalid decision type...');
    Result := True;
    for i := 0 to GenesNumber - 1 do
    begin
        SelectedGene := i;
        for j := 0 to ParametersNumber - 1 do
        begin
            if Abs(TwoDimFloatDecision[j] - Self[j]) >= TINY then
            begin
                Result := False;
                Exit;
            end;
        end;
    end;
end;

procedure TTwoDimFloatDecision.InvertParameter(const GeneNum, ParamNum: LongInt);
begin
    SelectedGene := GeneNum;
    Self[ParamNum] := (-1) * Self[ParamNum];
end;

procedure TTwoDimFloatDecision.ExchangeParameters(
    const GeneNum1, ParamNum1, GeneNum2, ParamNum2: LongInt);
var
    TempDouble1, TempDouble2: Double;
begin
    SelectedGene := GeneNum1;
    TempDouble1 := Self[ParamNum1];
    SelectedGene := GeneNum2;
    TempDouble2 := Self[ParamNum2];
    SelectedGene := GeneNum1;
    Self[ParamNum1] := TempDouble2;
    SelectedGene := GeneNum2;
    Self[ParamNum2] := TempDouble1;
end;

procedure TTwoDimFloatDecision.ExchangeWithOuter(const Decision: TAbstractDecision;
    MyGeneNum, OuterGeneNum, ParamNum: LongInt);
var
    TempDouble: Double;
begin
    if not (Decision is TTwoDimFloatDecision) then
        raise ETwoDimFloatDecision.Create('Invalid decision type ' + Decision.ClassName);

    Self.SelectedGene := MyGeneNum;
    TTwoDimFloatDecision(Decision).SelectedGene := OuterGeneNum;

    TempDouble := Self[ParamNum];
    Self[ParamNum] := TTwoDimFloatDecision(Decision)[ParamNum];
    TTwoDimFloatDecision(Decision)[ParamNum] := TempDouble;
end;

procedure TTwoDimFloatDecision.CopyParameter(
    const SrcGeneNum, SrcParamNum, DestGeneNum, DestParamNum: LongInt);
var
    TempDouble: Double;
begin
    SelectedGene := SrcGeneNum;
    TempDouble := Self[SrcParamNum];
    SelectedGene := DestGeneNum;
    Self[DestParamNum] := TempDouble;
end;

procedure TTwoDimFloatDecision.CopyBlock(
    const StartGeneNum, EndGeneNum, StartParamNum, EndParamNum,
    GeneOffset, ParamOffset: LongInt);
var
    SavedBlock: array of array of Double;
    i, j: LongInt;
    Index1, Index2: LongInt;
begin
    { Intermediate array is used because destination and source can override. }
    SetLength(SavedBlock, EndGeneNum - StartGeneNum + 1);
    for i := 0 to Length(SavedBlock) - 1 do
        SetLength(SavedBlock[i], EndParamNum - StartParamNum + 1);

    for i := 0 to Length(SavedBlock) - 1 do
        for j := 0 to Length(SavedBlock[i]) - 1 do
            SavedBlock[i, j] := FParameters[i + StartGeneNum, j + StartParamNum];

    for i := 0 to Length(SavedBlock) - 1 do
        for j := 0 to Length(SavedBlock[i]) - 1 do
        begin
            Index1 := StartGeneNum + GeneOffset + i;
            Index2 := StartParamNum + ParamOffset + j;

            if Index1 > GenesNumber - 1 then
                Index1 := Index1 - GenesNumber * (Index1 div GenesNumber);

            if Index1 < 0 then
                Index1 := (GenesNumber - 1) + (Index1 + GenesNumber *
                    (Abs(Index1) div GenesNumber));

            if Index2 > ParametersNumber - 1 then
                Index2 := Index2 - ParametersNumber * (Index2 div ParametersNumber);

            if Index2 < 0 then
                Index2 := (ParametersNumber - 1) +
                    (Index2 + ParametersNumber * (Abs(Index2) div ParametersNumber));

            FParameters[Index1, Index2] := SavedBlock[i, j];
        end;

    for i := 0 to Length(SavedBlock) - 1 do
        Finalize(SavedBlock[i]);
    Finalize(SavedBlock);
end;

procedure TTwoDimDecision.ExchangeBlocksWithOuter(const Decision: TAbstractDecision;
    StartGeneNum, EndGeneNum, StartParamNum, EndParamNum: LongInt);
var
    i, j: LongInt;
begin
    for i := StartGeneNum to EndGeneNum do
        for j := StartParamNum to EndParamNum do
            ExchangeWithOuter(Decision, i, i, j);
end;

procedure TTwoDimDecision.InvertBlock(
    const StartGeneNum, EndGeneNum, StartParamNum, EndParamNum: LongInt);
var
    i, j: LongInt;
begin
    for i := StartGeneNum to EndGeneNum do
        for j := StartParamNum to EndParamNum do
            InvertParameter(i, j);
end;

function TDecisionsList.GetMaxDecision(const StartIndex: LongInt;
    UpLimit: Double): TAbstractDecision;
var
    i: LongInt;
    Decision: TAbstractDecision;
    Max: Double;
    { Controls initialization of Max. }
    Flag: Boolean;
begin
    Result := nil;
    if Count = 0 then
        raise EDecisionsList.Create('Decisions list should not be empty...');

    Flag := True;
    { Warning suppression. }
    Max := 0;
    for i := StartIndex to Count - 1 do
    begin
        Decision := TAbstractDecision(Items[i]);
        if Flag then
        begin
            { Searches for the first value less or equal to UpLimit. }
            if Decision.Evaluation <= UpLimit then
            begin
                Max := Decision.Evaluation;
                Result := Decision;
                Flag := False;
                { Checks if the best solution found. }
                if Decision.Evaluation = UpLimit then
                    Exit;
            end;
        end
        else
        begin
            { Checks another solution if it would be be better than the first found. }
            if (Decision.Evaluation >= Max) and (Decision.Evaluation <= UpLimit) then
            begin
                Max := Decision.Evaluation;
                Result := Decision;
                { Checks if the best solution found. }
                if Decision.Evaluation = UpLimit then
                    Exit;
            end;
        end;
    end;
end;

function TDecisionsList.GetMinDecision(const StartIndex: LongInt;
    LowLimit: Double): TAbstractDecision;
var
    i: LongInt;
    Decision: TAbstractDecision;
    Min: Double;
    { Controls initialization of Min. }
    Flag: Boolean;
begin
    Result := nil;
    if Count = 0 then
        raise EDecisionsList.Create('Decisions list should not be empty...');

    Flag := True;
    { Warning suppression. Min is initialized when the first
      point is found having evaluation value greater or
      equal to the lower limit (see below). }
    Min := 0;

    for i := StartIndex to Count - 1 do
    begin
        Decision := TAbstractDecision(Items[i]);
        if Flag then
        begin
            { Searches for the first value greater or equal to LowLimit. }
            if Decision.Evaluation >= LowLimit then
            begin
                Min := Decision.Evaluation;
                Result := Decision;
                Flag := False;
                { Checks if the best solution found. }
                if Decision.Evaluation = LowLimit then
                    Exit;
            end;
        end
        else
        begin
            { Checks another solution if it would be be better than the first found. }
            if (Decision.Evaluation <= Min) and (Decision.Evaluation >= LowLimit) then
            begin
                Min := Decision.Evaluation;
                Result := Decision;
                { Checks if the best solution found. }
                if Decision.Evaluation = LowLimit then
                    Exit;
            end;
        end;
    end;
end;

function TDecisionsList.GetAbsoluteMin: TAbstractDecision;
var
    i: LongInt;
    Decision: TAbstractDecision;
begin
    if Count = 0 then
        raise EDecisionsList.Create('Decisions list should not be empty...');

    Decision := TAbstractDecision(Items[0]);
    Result := Decision;
    for i := 1 to Count - 1 do
    begin
        Decision := TAbstractDecision(Items[i]);
        if Decision.Evaluation < Result.Evaluation then
            Result := Decision;
    end;
end;

function TDecisionsList.GetAbsoluteMax: TAbstractDecision;
var
    i: LongInt;
    Decision: TAbstractDecision;
begin
    if Count = 0 then
        raise EDecisionsList.Create('Decisions list should not be empty...');

    Decision := TAbstractDecision(Items[0]);
    Result := Decision;
    for i := 1 to Count - 1 do
    begin
        Decision := TAbstractDecision(Items[i]);
        if Decision.Evaluation > Result.Evaluation then
            Result := Decision;
    end;
end;

function TDecisionsList.HasThisDecision(const Decision: TAbstractDecision): Boolean;
var
    i: LongInt;
    TempDecision: TAbstractDecision;
begin
    Result := False;
    for i := 0 to Count - 1 do
    begin
        TempDecision := TAbstractDecision(Items[i]);
        if TempDecision.Coincide(Decision) then
        begin
            Result := True;
            Exit;
        end;
    end;
end;

function EvalUpSortFunc(Item1, Item2: Pointer): Integer;
var
    Decision1: TAbstractDecision absolute Item1;
    Decision2: TAbstractDecision absolute Item2;
begin
    Result := 0;
    if Decision1.Evaluation > Decision2.Evaluation then
        Result := 1;
    if Decision1.Evaluation < Decision2.Evaluation then
        Result := -1;
end;

function EvalDownSortFunc(Item1, Item2: Pointer): Integer;
var
    Decision1: TAbstractDecision absolute Item1;
    Decision2: TAbstractDecision absolute Item2;
begin
    Result := 0;
    if Decision1.Evaluation > Decision2.Evaluation then
        Result := -1;
    if Decision1.Evaluation < Decision2.Evaluation then
        Result := 1;
end;

initialization
    RegisterClass(TFloatDecision);
    RegisterClass(TTwoDimFloatDecision);
    RegisterClass(TByteDecision);
end.
