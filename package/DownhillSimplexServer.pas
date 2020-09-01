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
unit DownhillSimplexServer;

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
        procedure UpdateResults(Sender: TComponent);
    end;

    EDownhillSimplexContainer = class(Exception);

    { Container is responsible for feeding algorithm with parameter
      values and limiting values by cyclical boundary conditions. }
    TDownhillSimplexServer = class(TAlgorithmContainer, IDownhillSimplexServer)
    protected
        { Array of interfaces used for getting parameters. }
        FParametersInterfaces: array of IDownhillRealParameters;
        FOptimizedFunction: IOptimizedFunction;
        FUpdatingResults: IUpdatingResults;

        FCombSelector: TCombSelector;

        FFinalTolerance: Double;
        FRestartDisabled: Boolean;
        FExitDerivative: Double;

        FEndOfCalculation: Boolean;
        FMessage: string;
        FTotalMinimumDecision: TFloatDecision;

        { Initializes environment and starts algorithm. }
        procedure Running; override;
        procedure RunningFinished; override;

        procedure ShowMessage;
        procedure SetTotatlMinimumDecision(ATotalMinimumDecision: TFloatDecision);

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

        function GetUpdatingResults: IUpdatingResults;
        function GetOptimizedFunction: IOptimizedFunction;

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
        FTotalMinimum: Double;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure StopAlgorithm; override;

        procedure ClearListOfIDSPs;
        procedure AddIDSPToList(const IDSP_: IDownhillRealParameters);

        property OptimizedFunction: IOptimizedFunction
            read GetOptimizedFunction write FOptimizedFunction;
        property UpdatingResults: IUpdatingResults
            read GetUpdatingResults write FUpdatingResults;

        { Container must save copies of these properties because during
          calculation process algorithm object can be created a few times. }
        property FinalTolerance: Double write FFinalTolerance;
        property RestartDisabled: Boolean write FRestartDisabled;
        { Stops calculation if minimal value changed less than on this value for optimization cycle. }
        property ExitDerivative: Double write FExitDerivative;
    end;

implementation

{$hints off}
function TDownhillSimplexServer.GetVariationStep(Sender: TComponent;
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

procedure TDownhillSimplexServer.FillStartDecision(Sender: TComponent;
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

procedure TDownhillSimplexServer.FillParameters(Decision: TFloatDecision);
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

procedure TDownhillSimplexServer.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
begin
    FillParameters(Decision);
    Decision.Evaluation := OptimizedFunction.GetOptimizedFunction;
end;

procedure TDownhillSimplexServer.UpdateResults(Sender: TComponent;
    Decision: TFloatDecision);
begin
    if Decision.Evaluation < FTotalMinimum then
    begin
        FTotalMinimum := Decision.Evaluation;
        SetTotatlMinimumDecision(TFloatDecision(Decision.GetCopy));
        UpdatingResults.UpdateResults(Self);
    end;
end;

procedure TDownhillSimplexServer.SetTotatlMinimumDecision(
    ATotalMinimumDecision: TFloatDecision);
begin
    if Assigned(FTotalMinimumDecision) then
        FTotalMinimumDecision.Free;
    FTotalMinimumDecision := ATotalMinimumDecision;
end;

function TDownhillSimplexServer.EndOfCalculation(Sender: TComponent): Boolean;
begin
    Result := FEndOfCalculation;
end;

constructor TDownhillSimplexServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FCombSelector := TCombSelector.Create;
end;

destructor TDownhillSimplexServer.Destroy;
begin
    StopAlgorithm;
    DestroyAlgorithm;

    ClearListOfIDSPs;
    UtilizeObject(FCombSelector);
    inherited Destroy;
end;

procedure TDownhillSimplexServer.ClearListOfIDSPs;
var
    i: LongInt;
begin
    for i := 0 to Length(FParametersInterfaces) - 1 do
        FParametersInterfaces[i] := nil;    //  dlya umen'sheniya schetchika ssylok
    //  ??? budut li schetchiki ssylok korrektno
    //  umen'shat'sya, esli prosto vyzyvat' Finalize
    Finalize(FParametersInterfaces);
    FCombSelector.ClearDiscretValuesList;
end;

procedure TDownhillSimplexServer.AddIDSPToList(
    const IDSP_: IDownhillRealParameters);
begin
    SetLength(FParametersInterfaces, Length(FParametersInterfaces) + 1);
    FParametersInterfaces[Length(FParametersInterfaces) - 1] := IDSP_;
    FCombSelector.AddDiscretValue(IDSP_);
end;

function TDownhillSimplexServer.GetIDSPsNumber: LongInt;
begin
    Result := Length(FParametersInterfaces);
end;

function TDownhillSimplexServer.GetIDSP(index: LongInt): IDownhillRealParameters;
begin
    Result := FParametersInterfaces[index];
end;

function TDownhillSimplexServer.GetUpdatingResults: IUpdatingResults;
begin
    if Assigned(FUpdatingResults) then
        Result := FUpdatingResults
    else
        raise EDownhillSimplexContainer.Create(
            'Updating results interface must be assigned...');
end;

function TDownhillSimplexServer.GetOptimizedFunction: IOptimizedFunction;
begin
    if Assigned(FOptimizedFunction) then
        Result := FOptimizedFunction
    else
        raise EDownhillSimplexContainer.Create(
            'Optimized function interface must be assigned...');
end;

procedure TDownhillSimplexServer.StopAlgorithm;
begin
    FEndOfCalculation := True;
end;

procedure TDownhillSimplexServer.DestroyAlgorithm;
begin
    UtilizeObject(Algorithm);
end;

procedure TDownhillSimplexServer.ShowMessage;
begin
    UpdatingResults.ShowMessage(Self, FMessage);
end;

procedure TDownhillSimplexServer.RunningFinished;
begin
    FMessage := 'Calculation done...';
    ShowMessage;
end;

procedure TDownhillSimplexServer.CreateAlgorithm;
begin
    UtilizeObject(Algorithm);
    Algorithm := TDownhillSimplexAlgorithm.Create(nil,
        FFinalTolerance, FRestartDisabled, FExitDerivative);
    //    Algorithm := TDownhillSimplexSAAlgorithm.Create(nil);
    with Algorithm as TDownhillSimplexAlgorithm do
        //    with Algorithm as TDownhillSimplexSAAlgorithm do
    begin
        DownhillSimplexServer := Self;
        //  Temperature := 1;     //  for TDownhillSimplexSAAlgorithm
    end;
end;

procedure TDownhillSimplexServer.Running;
var
    i: LongInt;
begin
    FTotalMinimum := OptimizedFunction.GetOptimizedFunction;
    UpdatingResults.ResetCurJobProgress(Self);
    UpdatingResults.ShowCurJobProgress(Self, 0, FCombSelector.CombNumber, 0);
    for i := 0 to FCombSelector.CombNumber - 1 do
    begin
        if FEndOfCalculation then
            Exit;
        FCombSelector.CurrentComb := i;
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
        UpdatingResults.ShowCurJobProgress(Self, 0, FCombSelector.CombNumber, i + 1);
    end;
end;

function TDownhillSimplexServer.GetParametersNumber: LongInt;
var
    i: LongInt;
begin
    Result := 0;
    for i := 0 to IDSPsNumber - 1 do
        Result := Result + IDSP[i].ParametersNumber;
end;

procedure TDownhillSimplexServer.CreateParameters;
var
    i: LongInt;
begin
    for i := 0 to IDSPsNumber - 1 do
        IDSP[i].CreateParameters;
end;

function TDownhillSimplexServer.GetParameter(index: LongInt): TVariableParameter;
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

procedure TDownhillSimplexServer.SetParameter(index: LongInt;
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
