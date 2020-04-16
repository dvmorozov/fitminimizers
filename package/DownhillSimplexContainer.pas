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
    SimpMath, CombEnumerator, Tools;

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

        function GetVariationStep(index: LongInt): double;
        procedure SetVariationStep(index: LongInt; Value: double);

        { Total number of variable parameters. }
        property ParametersNumber: LongInt read GetParametersNumber;
        property Parameter[index: LongInt]: TVariableParameter
            read GetParameter write SetParameter;
        property VariationStep[index: LongInt]: double
            read GetVariationStep write SetVariationStep;
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

    EDownhillSimplexContainer = class(Exception);

    { Container is responsible for feeding algorithm with parameter
      values and limiting values by cyclical boundary conditions. }
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

        FEndOfCalculation: Boolean;
        FMessage: string;
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

        { IDownhillSimplexServer implementation. }
        { Returns initial characteristic length for every parameter. }
        function GetVariationStep(Sender: TComponent; index: LongInt): Double;
        { Fills coordinates of initial simplex vertex. Only parameters
          are set up, the method doesn't compute goal function! }
        procedure FillStartDecision(Sender: TComponent; StartDecision: TFloatDecision);
        { Calculates function value for set of parameters of solution object. }
        procedure EvaluateDecision(Sender: TComponent; Decision: TFloatDecision);
        procedure UpdateResults(Sender: TComponent; Decision: TFloatDecision);
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
function TDownhillSimplexContainer.GetVariationStep(Sender: TComponent;
    index: LongInt): Double;
var
    i: LongInt;
    ParameterNumber: LongInt;
    ParameterCount: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');

    ParameterCount := 0;
    for i := 0 to IDSPsNumber - 1 do
    begin
        ParameterNumber := IDSP[i].ParametersNumber;
        //  Searches underlying component responsible for that parameter.
        if (index >= ParameterCount) and (index < ParameterCount + ParameterNumber) then
        begin
            Result := IDSP[i].VariationStep[index - ParameterCount];
            Exit;
        end
        else
            ParameterCount := ParameterCount + ParameterNumber;
    end;

    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

{$hints on}

procedure TDownhillSimplexContainer.FillStartDecision(Sender: TComponent;
    StartDecision: TFloatDecision);
var
    i: LongInt;
    CurParameter: TVariableParameter;
begin
    StartDecision.ParametersNumber := ParametersNumber;
    for i := 0 to ParametersNumber - 1 do
    begin
        CurParameter := Parameter[i];
        with CurParameter do
            if Limited and not IsValueIntoInterval(MinLimit, MaxLimit, Value) then
                //  konechno, parametr mozhno bylo by ispravit' i zdes', no vse zhe
                //  eto zadacha klienta - predostavit' pravil'nye nachal'nye parametry
                raise EDownhillSimplexContainer.Create(
                    'Parameter value is not into the interval...')
            else
                StartDecision.Parameters[i] := Value;
    end;{for i := 0 to TempParametersNumber - 1 do...}
end;

procedure TDownhillSimplexContainer.FillParameters(Decision: TFloatDecision);
var
    i: LongInt;
    CurParameter: TVariableParameter;
begin
    for i := 0 to ParametersNumber - 1 do
    begin
        CurParameter := Parameter[i];
        //  Copies new parameter value.
        CurParameter.Value := Decision.Parameters[i];
        with CurParameter do
            if Limited then
                PutValueIntoInterval(MinLimit, MaxLimit, Value);

        Parameter[i] := CurParameter;
    end;

    //  Notifies underlying components about CurParameter changing.
    for i := 0 to IDSPsNumber - 1 do
    begin
        IDSP[i].ParametersUpdated;
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
    EvaluateDecision(Sender, Decision);
    //  pereschet resheniya zdes' neobhodim, tak kak v dannom
    //  algoritme luchshiy rezul'tat ne obyazatel'no posledniy
    //  (posle restarta)
    //??? proverit', nuzhno li pereschityvat' - Evaluation d.
    //  hranit' znachenie
    if Decision.Evaluation < TotalMinimum then
    begin
        TotalMinimum := Decision.Evaluation;
        UpdateMainForm;
    end;
end;

function TDownhillSimplexContainer.EndOfCalculation(Sender: TComponent): Boolean;
begin
    Result := FEndOfCalculation;
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
        FParametersInterfaces[i] := nil;    //  dlya umen'sheniya schetchika ssylok
    //  ??? budut li schetchiki ssylok korrektno
    //  umen'shat'sya, esli prosto vyzyvat' Finalize
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
    FEndOfCalculation := True;
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
    UpdatingResults.ShowMessage(Self, FMessage);
end;

procedure TDownhillSimplexContainer.RunningFinished;
begin
    FMessage := 'Calculation done...';
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
        //  Final tolerance should have non zero value,
        //  otherwise computation will never end.
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
        if FEndOfCalculation then
            Exit;
        CombSelector.CurrentComb := i;
        CreateParameters;
        //  sozdayutsya parametry dlya novoy kombinatsii
        if ParametersNumber <> 0 then
        begin
            CreateAlgorithm;
            Algorithm.AlgorithmRealization;
        end
        else
        begin
            FMessage := 'List of parameters is empty for combination ' + IntToStr(i);
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
    ParameterNumber: LongInt;
    ParameterCount: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');

    ParameterCount := 0;
    for i := 0 to IDSPsNumber - 1 do
    begin
        ParameterNumber := IDSP[i].ParametersNumber;
        //  Searches underlying component responsible for that parameter.
        if (index >= ParameterCount) and (index < ParameterCount + ParameterNumber) then
        begin
            Result := IDSP[i].Parameter[index - ParameterCount];
            Exit;
        end
        else
            ParameterCount := ParameterCount + ParameterNumber;
    end;

    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

procedure TDownhillSimplexContainer.SetParameter(index: LongInt;
    AParameter: TVariableParameter);
var
    i: LongInt;
    ParameterNumber: LongInt;
    ParameterCount: LongInt;
begin
    if (index < 0) then
        raise EDownhillSimplexContainer.Create('Invalid parameter index...');
    ParameterCount := 0;
    for i := 0 to IDSPsNumber - 1 do
    begin
        ParameterNumber := IDSP[i].ParametersNumber;
        //  poisk interfeysa v parametry kotorogo popadaet indeks
        if (index >= ParameterCount) and (index < ParameterCount + ParameterNumber) then
        begin
            IDSP[i].Parameter[index - ParameterCount] := AParameter;
            Exit;
        end
        else
            ParameterCount := ParameterCount + ParameterNumber;
    end;
    raise EDownhillSimplexContainer.Create('Invalid parameter index...');
end;

initialization
end.
