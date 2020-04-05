{
This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                    LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                    Facebook: https://www.facebook.com/dmitry.v.morozov

@abstract(Contains definitions of container classes used for optimization.)

@author(Dmitry Morozov dvmorozov@hotmail.com,
LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/,
Facebook: https://www.facebook.com/dmitry.v.morozov)
}
unit DownhillSimplexContainer;

interface

uses
    Classes, DownhillSimplexAlgorithm, AlgorithmContainer, Decisions, SysUtils,
    SimpMath, CombEnumerator, CBRCComponent, Tools;

const
    { Possible states of parameters in TDownhillRealParameters. }
    { Waiting for call of CreateParameters. }
    PH_CREATING = 0;
    { Processing. }
    PH_WORKING = 1;

type
    TVariableParameter = record
        Value: Double;
        { If True possible values of parameter are limited by MaxLimit, MinLimit. }
        Limited: Boolean;
        { Maximal possible value of parameter. Is used if Limited is True. }
        MaxLimit: Double;
        { Minimal possible value of parameter. Is used if Limited is True. }
        MinLimit: Double;
    end;

    { Provides access to variable parameters to algorithm container. }
    IDownhillSimplexParameters = interface(IDiscretValue)
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);
        { Total number of variable parameters. }
        property ParametersNumber: LongInt read GetParametersNumber;
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter write SetParameter;
    end;

    { Provides additional methods to work with parameters. }
    IDownhillRealParameters = interface(IDownhillSimplexParameters)
        { Initializing parameter list. }
        procedure CreateParameters;
        { Finalizing cycle of parameter updating. }
        procedure ParametersUpdated;
    end;

    { Provides values of optimized function. }
    IOptimizedFunction = interface
        function GetOptimizedFunction: Double;
    end;

    { Provides way of displaying results. }
    IUpdatingResults = interface
        procedure ShowCurJobProgress(Sender: TComponent;
            MinValue, MaxValue, CurValue: LongInt);
        procedure ResetCurJobProgress(Sender: TComponent);
        procedure ShowMessage(Sender: TComponent; Msg: string);
        procedure UpdatingResults(Sender: TComponent);
    end;

    EDownhillRealParameters = class(Exception);

    { Container for algorithm parameters. }
    TDownhillRealParameters = class(TCBRCComponent, IDownhillRealParameters)
    protected
        { Pointer to parameter array. }
        FParameters: Pointer;
        { Total number of parameters in array. }
        FParametersNumber: LongInt;
        { Current state of parameter processing. }
        PhaseParameters: Byte;

        procedure CreateParameters; virtual;
        procedure FreeParameters;
        { Initializes parameters. }
        procedure FillParameters; virtual; abstract;
        { Converts parameters from array to components of vectors of magnetic moments. }
        procedure ParametersUpdated; virtual; abstract;
        { Returns actual number of parameters. }
        function GetActualParametersNumber: LongInt; virtual; abstract;
        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);

        function GetNumberOfValues: LongInt; virtual; abstract;
        function GetValueIndex: LongInt; virtual; abstract;
        procedure SetValueIndex(const AValueIndex: LongInt); virtual; abstract;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        { Implementation of IDownhillSimplexParameters }
        { Number of variable parameters. }
        property ParametersNumber: LongInt read GetParametersNumber;
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter write SetParameter;
        { Implementation of IDiscretValue }
        { Number of discrete values which the quantity can take. }
        property NumberOfValues: LongInt read GetNumberOfValues;
        { Index of discrete value selected at this moment. }
        property ValueIndex: LongInt read GetValueIndex write SetValueIndex;
    end;

    EDownhillSimplexContainer = class(Exception);

    { Container is responsible for feeding algorithm with parameters 
      and limiting values of parameters by cyclical boundary conditions. }
    TDownhillSimplexContainer = class(TAlgorithmContainer, IDownhillSimplexServer)
    protected
        { Array of interfaces used for getting parameters. }
        FParametersInterfaces: array of IDownhillRealParameters;
        FIOptimizedFunction: IOptimizedFunction;
        FIUpdatingResults: IUpdatingResults;

        CombSelector: TCombSelector;

        FFinalTolerance: Double;
        FRestartDisabled: Boolean;
        FExitDerivative: Double;

        EOC: Boolean;
        message: string;
        { Initializes environment and starts algorithm. }
        procedure Running; override;
        procedure RunningFinished; override;

        { Wrapping method. It doesn't have parameters because is called by Synchronize. }
        procedure ShowMessage;
        { Wrapping method. It doesn't have parameters because is called by Synchronize. }
        procedure UpdateMainForm;

        procedure FillParameters(Decision: TFloatDecision); virtual;
        procedure CreateAlgorithm; override;
        procedure CreateParameters;
        procedure DestroyAlgorithm; override;

        { Returns initial characteristic length for every parameter. }
        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double; virtual;
        { Fills coordinates of initial simplex vertex. Only parameters
          are set up, the method doesn't compute goal function! }
        procedure FillStartDecision(Sender: TComponent;
            StartDecision: TFloatDecision); virtual;
        { Calculates function value for set of parameters of solution object. }
        procedure EvaluateDecision(Sender: TComponent;
            Decision: TFloatDecision); virtual;
        procedure UpdateResults(Sender: TComponent;
            Decision: TFloatDecision); virtual;
        { Calculates condition of calculation termination. }
        function EndOfCalculation(Sender: TComponent): Boolean;

        function GetIUpdatingResults: IUpdatingResults;
        function GetIOptimizedFunction: IOptimizedFunction;

        function GetIDSPsNumber: LongInt;
        function GetIDSP(index: LongInt): IDownhillRealParameters;
        property IDSPsNumber: LongInt read GetIDSPsNumber;
        property IDSP[index: LongInt]: IDownhillRealParameters read GetIDSP;

        function GetParametersNumber: LongInt;
        function GetParameter(index: LongInt): TVariableParameter;
        procedure SetParameter(index: LongInt; AParameter: TVariableParameter);
        { Total number of variable parameters. }
        property ParametersNumber: LongInt read GetParametersNumber;
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter write SetParameter;

    public
        { Overall minimum among all optimization cycles. }
        TotalMinimum: Double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure StopAlgorithm; override;

        procedure ClearListOfIDSPs;
        procedure AddIDSPToList(const IDSP_: IDownhillRealParameters);

        property OptimizedFunction: IOptimizedFunction
            read GetIOptimizedFunction write FIOptimizedFunction;
        property UpdatingResults: IUpdatingResults
            read GetIUpdatingResults write FIUpdatingResults;

        { Container must save copies of these properties because during
          calculation process algorithm object can be created a few times. }
        property FinalTolerance: Double read FFinalTolerance write FFinalTolerance;
        property RestartDisabled: Boolean read FRestartDisabled write FRestartDisabled;
        { Stops calculation if minimal value changed less than on this value for optimization cycle. }
        property ExitDerivative: Double read FExitDerivative write FExitDerivative;
    end;

implementation

type
    TParametersArray = array[0..MaxInt div SizeOf(TVariableParameter) - 1] of
        TVariableParameter;

{$hints off}
function TDownhillSimplexContainer.GetInitParamLength(Sender: TComponent;
    ParameterNumber, ParametersCount: LongInt): Double;
begin
    Result := 0.1;
end;

{$hints on}

procedure TDownhillSimplexContainer.FillStartDecision(Sender: TComponent;
    StartDecision: TFloatDecision);
var
    i: LongInt;
    TempParamsNumber: LongInt;
    TempParameter: TVariableParameter;
begin
    TempParamsNumber := ParametersNumber;
    StartDecision.ParametersNumber := TempParamsNumber;
    for i := 0 to TempParamsNumber - 1 do
    begin
        TempParameter := Parameter[i];
        with TempParameter do
            if Limited and not IsValueIntoInterval(MinLimit, MaxLimit, Value) then
                raise EDownhillSimplexContainer.Create(
                    'Parameter value is not into the interval...')
            else
                StartDecision.Parameters[i] := Value;
    end; { for i := 0 to TempParametersNumber - 1 do...}
end;

procedure TDownhillSimplexContainer.FillParameters(Decision: TFloatDecision);
var
    i: LongInt;
    TempParamsNumber: LongInt;
    TempParameter: TVariableParameter;
    TempIDSP: IDownhillRealParameters;
begin
    TempParamsNumber := ParametersNumber;
    for i := 0 to TempParamsNumber - 1 do
    begin
        TempParameter := Parameter[i];
        TempParameter.Value := Decision.Parameters[i];
        { Puts value into given interval. }
        with TempParameter do
            if Limited then
                PutValueIntoInterval(MinLimit, MaxLimit, Value);
        Parameter[i] := TempParameter;
    end;

    for i := 0 to IDSPsNumber - 1 do
    begin
        TempIDSP := IDSP[i];
        TempIDSP.ParametersUpdated;
    end;
end;

procedure TDownhillSimplexContainer.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
begin
    FillParameters(Decision);
    Decision.Evaluation := OptimizedFunction.GetOptimizedFunction;
end;

procedure TDownhillSimplexContainer.UpdateResults(Sender: TComponent;
    Decision: TFloatDecision);
begin
    { TODO: make sure that recomputing is actually necessary.
      Decision.Evaluation should contain computed value. }
    EvaluateDecision(Sender, Decision);
    if Decision.Evaluation < TotalMinimum then
    begin
        TotalMinimum := Decision.Evaluation;
        UpdateMainForm;
    end;
end;

function TDownhillSimplexContainer.EndOfCalculation(Sender: TComponent): Boolean;
begin
    Result := EOC;
end;

constructor TDownhillSimplexContainer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    CombSelector := TCombSelector.Create;
end;

destructor TDownhillSimplexContainer.Destroy;
begin
    StopAlgorithm;
    DestroyAlgorithm;

    ClearListOfIDSPs;
    UtilizeObject(CombSelector);
    inherited Destroy;
end;

procedure TDownhillSimplexContainer.ClearListOfIDSPs;
var
    i: LongInt;
begin
    for i := 0 to Length(FParametersInterfaces) - 1 do
        { Decrements reference counter. }
        FParametersInterfaces[i] := nil;
    Finalize(FParametersInterfaces);
    CombSelector.ClearDiscretValuesList;
end;

procedure TDownhillSimplexContainer.AddIDSPToList(
    const IDSP_: IDownhillRealParameters);
begin
    SetLength(FParametersInterfaces, Length(FParametersInterfaces) + 1);
    FParametersInterfaces[Length(FParametersInterfaces) - 1] := IDSP_;
    CombSelector.AddDiscretValue(IDSP_);
end;

function TDownhillSimplexContainer.GetIDSPsNumber: LongInt;
begin
    Result := Length(FParametersInterfaces);
end;

function TDownhillSimplexContainer.GetIDSP(index: LongInt): IDownhillRealParameters;
begin
    Result := FParametersInterfaces[index];
end;

function TDownhillSimplexContainer.GetIUpdatingResults: IUpdatingResults;
begin
    if Assigned(FIUpdatingResults) then
        Result := FIUpdatingResults
    else
        raise EDownhillSimplexContainer.Create(
            'Updating results interface must be assigned...');
end;

function TDownhillSimplexContainer.GetIOptimizedFunction: IOptimizedFunction;
begin
    if Assigned(FIOptimizedFunction) then
        Result := FIOptimizedFunction
    else
        raise EDownhillSimplexContainer.Create(
            'Optimized function interface must be assigned...');
end;

procedure TDownhillSimplexContainer.StopAlgorithm;
begin
    EOC := True;
end;

procedure TDownhillSimplexContainer.DestroyAlgorithm;
begin
    UtilizeObject(Algorithm);
end;

procedure TDownhillSimplexContainer.UpdateMainForm;
begin
    UpdatingResults.UpdatingResults(Self);
end;

procedure TDownhillSimplexContainer.ShowMessage;
begin
    UpdatingResults.ShowMessage(Self, message);
end;

procedure TDownhillSimplexContainer.RunningFinished;
begin
    message := 'Calculation done...';
    ShowMessage;
end;

procedure TDownhillSimplexContainer.CreateAlgorithm;
begin
    UtilizeObject(Algorithm);
    Algorithm := TDownhillSimplexAlgorithm.Create(nil);
    //    Algorithm := TDownhillSimplexSAAlgorithm.Create(nil);
    with Algorithm as TDownhillSimplexAlgorithm do
        //    with Algorithm as TDownhillSimplexSAAlgorithm do
    begin
        DownhillSimplexServer := Self;
        { Final tolerance should have non zero value,
          otherwise computation will never end. }
        FinalTolerance := Self.FinalTolerance;
        RestartDisabled := Self.RestartDisabled;
        ExitDerivative := Self.ExitDerivative;
        //  Temperature := 1;     //  for TDownhillSimplexSAAlgorithm
    end;
end;

procedure TDownhillSimplexContainer.Running;
var
    i: LongInt;
begin
    TotalMinimum := OptimizedFunction.GetOptimizedFunction;
    UpdatingResults.ResetCurJobProgress(Self);
    UpdatingResults.ShowCurJobProgress(Self, 0, CombSelector.CombNumber, 0);
    for i := 0 to CombSelector.CombNumber - 1 do
    begin
        if EOC then
            Exit;
        CombSelector.CurrentComb := i;
        CreateParameters;
        if ParametersNumber <> 0 then
        begin
            CreateAlgorithm;
            Algorithm.AlgorithmRealization;
        end
        else
        begin
            message := 'List of parameters is empty for combination ' + IntToStr(i);
            ShowMessage;
        end;
        UpdatingResults.ShowCurJobProgress(Self, 0, CombSelector.CombNumber, i + 1);
    end;
end;

function TDownhillSimplexContainer.GetParametersNumber: LongInt;
var
    i: LongInt;
begin
    Result := 0;
    for i := 0 to IDSPsNumber - 1 do
        Result := Result + IDSP[i].ParametersNumber;
end;

procedure TDownhillSimplexContainer.CreateParameters;
var
    i: LongInt;
begin
    for i := 0 to IDSPsNumber - 1 do
        IDSP[i].CreateParameters;
end;

function TDownhillSimplexContainer.GetParameter(index: LongInt): TVariableParameter;
var
    i: LongInt;
    TempIDSP: IDownhillRealParameters;
    TempParamNumber: LongInt;
    ParamSum: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');
    ParamSum := 0;
    for i := 0 to IDSPsNumber - 1 do
    begin
        TempIDSP := IDSP[i];
        TempParamNumber := TempIDSP.ParametersNumber;
        { Searching of object which provides given parameter. }
        if (index >= ParamSum) and (index < ParamSum + TempParamNumber) then
        begin
            Result := TempIDSP.Parameter[index - ParamSum];
            Exit;
        end
        else
            ParamSum := ParamSum + TempParamNumber;
    end;
    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

procedure TDownhillSimplexContainer.SetParameter(index: LongInt;
    AParameter: TVariableParameter);
var
    i: LongInt;
    TempIDSP: IDownhillRealParameters;
    TempParamNumber: LongInt;
    ParamSum: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');
    ParamSum := 0;
    for i := 0 to IDSPsNumber - 1 do
    begin
        TempIDSP := IDSP[i];
        TempParamNumber := TempIDSP.ParametersNumber;
        { Searching of object which provides given parameter. }
        if (index >= ParamSum) and (index < ParamSum + TempParamNumber) then
        begin
            TempIDSP.Parameter[index - ParamSum] := AParameter;
            Exit;
        end
        else
            ParamSum := ParamSum + TempParamNumber;
    end;
    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

procedure TDownhillRealParameters.CreateParameters;
var
    ActParNum: LongInt;
begin
    FreeParameters;
    ActParNum := GetActualParametersNumber;
    if ActParNum <> 0 then
    begin
        GetMem(FParameters, ActParNum * SizeOf(TVariableParameter));
        FParametersNumber := ActParNum;
        PhaseParameters := PH_WORKING;
        { Phase should be set up before call of FillParameters. }
        FillParameters;
    end
    else
    begin
        FParameters := nil;
        FParametersNumber := 0;
        PhaseParameters := PH_WORKING;
    end;
end;

procedure TDownhillRealParameters.FreeParameters;
begin
    if PhaseParameters = PH_WORKING then
        if Assigned(FParameters) then
            FreeMem(FParameters(*, FParametersNumber * SizeOf(TVariableParameter)*));
end;

function TDownhillRealParameters.GetParameter(index: LongInt): TVariableParameter;
begin
    if (Index < 0) or (Index >= ParametersNumber) then
        raise EDownhillRealParameters.Create('Invalid parameter index...')
    else
        Result := TParametersArray(FParameters^)[index];
end;

procedure TDownhillRealParameters.SetParameter(index: LongInt;
    AParameter: TVariableParameter);
begin
    if (Index < 0) or (Index >= ParametersNumber) then
        raise EDownhillRealParameters.Create('Invalid parameter index...')
    else
        TParametersArray(FParameters^)[index] := AParameter;
end;

function TDownhillRealParameters.GetParametersNumber: LongInt;
begin
    case PhaseParameters of
        PH_CREATING: raise EDownhillRealParameters.Create(
                'Parameters must be created...');
        PH_WORKING: Result := FParametersNumber;
        else
            raise EDownhillRealParameters.Create('Invalid phase number...');
    end;
end;

constructor TDownhillRealParameters.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    PhaseParameters := PH_CREATING;
end;

destructor TDownhillRealParameters.Destroy;
begin
    FreeParameters;
    inherited Destroy;
end;

initialization
end.
