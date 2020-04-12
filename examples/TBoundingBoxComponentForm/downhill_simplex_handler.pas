unit downhill_simplex_handler;

interface

uses
{$IF NOT DEFINED(FPC)}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.Buttons, System.StrUtils,
{$ELSE}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
    StdCtrls,
{$ENDIF}
    Algorithm, DownhillSimplexAlgorithm, Decisions, SimpMath, Math3d;

type
    TDownHillSimplexHandler = class;
    { External method displaying attributes of container instance. }
    THandlerOutputProcedure = procedure(Handler: TDownHillSimplexHandler) of object;

    TDownHillSimplexHandler = class(TComponent, IDownhillSimplexServer)
    private
        { Minimum bounding box problem. }
        FDownhillSimplexAlgorithm: TDownhillSimplexAlgorithm;
        FHandlerOutputProcedure: THandlerOutputProcedure;

        FStop: Boolean;
        FShowAlgoDetails: Boolean;

        { Optimized values of angles describing rotation of coordinate system (in degrees). }
        FAlpha, FBeta, FGamma: Double;
        { Original values of angles describing rotation of coordinate system (in degrees). }
        FOriginalAlpha, FOriginalBeta, FOriginalGamma: Double;
        FRecreateSimplexFromOriginal: Boolean;

        FInitParamLength: Double;

        { Box infomations: volume and coordinates}
        FBoxVolume: Double;
        { Vectors containing triplets of maximum and minimum coordinates of
          model points. They are used to compute bounding box volume. }
        FBoxMinCoords, FBoxMaxCoords: TDoubleVector3;
        { Computation time. }
        FComputationTime: Single;
        { Unique container id. It is used only to reference results. }
        FRunId: Integer;
        { Measuring computation time. }
        FPerformanceFrequency, FStartTime: Int64;

        function GetCycleCount: Integer;
        function GetEvaluationCount: Integer;
        function GetRestartCount: Integer;

        { IDownhillSimplexServer implementation. }

        { Returns initial characteristic length for every parameter. }
        function GetInitialParameterStep(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double;
        { Sets inital calculation point in internal representation.
          The number of array element is equal to the number of
          variable parameters of task being solved. }
        procedure FillStartDecision(Sender: TComponent; iStartDecision: TFloatDecision);
        { Calculates evaluation function for the point given in internal
          representation. }
        procedure EvaluateDecision(Sender: TComponent; iDecision: TFloatDecision);
        { Displays current minimum. }
        procedure UpdateResults(Sender: TComponent; iDecision: TFloatDecision);
        { Returns flag of calculation termination. }
        function EndOfCalculation(Sender: TComponent): Boolean;
        { Starts measuring computation time. }
        procedure StartTimeMeasurement;
        { Ends measuring computation time. }
        procedure EndTimeMeasurement;

    public
        { If set simplex is recreated from original point on restarting,
          otherwise from the best point found during last optimization cycle. }
        constructor Create(AOwner: TComponent; iAlpha, iBeta, iGamma,
            iAlgoInitialStepsAngles: Double;
            iFinalTolerance, iExitDerivative: Double;
            iShowDetails: Boolean; RunId: Integer); reintroduce;
        destructor Destroy; override;
        { Initializes performance counters and starts optimization.
          The procedure should not have parameters because it is called
          from separate thread. }
        procedure OptimizeBoundingBox;
        { Interrupts computing. }
        procedure Stop;
        { Displays results of optimization.
          The procedure should not have parameters because it is called from
          separate thread. This is wrapper for call of FHandlerOutputProcedure. }
        procedure DisplayOutput;
        { Optimized values of angles describing rotation of coordinate system (in degrees). }
        property Alpha: Double read FAlpha;
        property Beta: Double read FBeta;
        property Gamma: Double read FGamma;
        { Box infomations: volume and coordinates. }
        property BoxVolume: Double read FBoxVolume;
        property BoxMinCoords: TDoubleVector3 read FBoxMinCoords;
        property BoxMaxCoords: TDoubleVector3 read FBoxMaxCoords;
        { DownHillSimplex algorithm statistical details. }
        property CycleCount: Integer read GetCycleCount;
        property EvaluationCount: Integer read GetEvaluationCount;
        property RestartCount: Integer read GetRestartCount;
        property HandlerOutputProcedure: THandlerOutputProcedure
            write FHandlerOutputProcedure;
        property ComputationTime: Single read FComputationTime;
        property RunId: Integer read FRunId;
    end;

function DegToRad(iDeg: Double): Double;

implementation

uses bounding_box_component_form;

function DegToRad(iDeg: Double): Double;
begin
    Result := iDeg * PI / 180.0;
end;

function ComputeRotatedBoxVolume(iAlpha, iBeta, iGamma: Single;
    var iMinCoords, iMaxCoords: T3Vector): Double;

    function GetRotationMatrix(iAlpha, iBeta, iGamma: Single): TMatrix;
    var
        fRotX, fRotY, fRotZ, fMatr: TMatrix;
    begin
        { Computing rotation matrices.
          Matrices are initalized inside functions. }
        fRotX := MatrixRotX(DegToRad(iAlpha));
        fRotY := MatrixRotY(DegToRad(iBeta));
        fRotZ := MatrixRotZ(DegToRad(iGamma));
        { Computes rotation matrix. }
        fMatr := UnitMatrix;
        Mul3DMatrix(fRotZ, fMatr, fMatr);
        Mul3DMatrix(fRotY, fMatr, fMatr);
        Mul3DMatrix(fRotX, fMatr, fMatr);
        Result := fMatr;
    end;

var
    i: Integer;
    fA, fB, fC: Double; //  Sizes of the box.
    fMatr: TMatrix;
    fPoint: p3DVector;
    fVector: T3Vector;
begin
    { Computes volume of bounding box. }
    fMatr := GetRotationMatrix(iAlpha, iBeta, iGamma);
    fPoint := PointCloud[0];
    fVector := fPoint^.fVector;
    MulVectMatr(fMatr, fVector);
    iMaxCoords := fVector;
    iMinCoords := fVector;
    for i := 1 to PointCloud.Count - 1 do
    begin
        fPoint := PointCloud[i];
        fVector := fPoint^.fVector;
        MulVectMatr(fMatr, fVector);
        if fVector[1] > iMaxCoords[1] then
            iMaxCoords[1] := fVector[1];
        if fVector[2] > iMaxCoords[2] then
            iMaxCoords[2] := fVector[2];
        if fVector[3] > iMaxCoords[3] then
            iMaxCoords[3] := fVector[3];
        if fVector[1] < iMinCoords[1] then
            iMinCoords[1] := fVector[1];
        if fVector[2] < iMinCoords[2] then
            iMinCoords[2] := fVector[2];
        if fVector[3] < iMinCoords[3] then
            iMinCoords[3] := fVector[3];
    end;
    fA := iMaxCoords[1] - iMinCoords[1];
    fB := iMaxCoords[2] - iMinCoords[2];
    fC := iMaxCoords[3] - iMinCoords[3];
    Result := fA * fB * fC;
end;

procedure DisplayDetails(iString: string);
begin
    BoundingBoxComponentForm.Memo2.Lines.Add(iString);
end;

//-----------------------------------------------------------------------------
//-------------------------- TDownHillSimplexHandler --------------------------
//-----------------------------------------------------------------------------

constructor TDownHillSimplexHandler.Create(
    AOwner: TComponent; iAlpha, iBeta, iGamma,
    iAlgoInitialStepsAngles: Double;
    iFinalTolerance, iExitDerivative: Double;
    iShowDetails: Boolean; RunId: Integer);
begin
    inherited Create(AOwner);
    FDownhillSimplexAlgorithm := TDownhillSimplexAlgorithm.Create(Self);
    { Initializing algorithm exit parameters. }
    FDownhillSimplexAlgorithm.FinalTolerance := 0.00001;
    FDownhillSimplexAlgorithm.ExitDerivative := 0.5;
    FDownhillSimplexAlgorithm.RestartDisabled := False;
    { FDownhillSimplexAlgorithm.SimplexDirectionChangingEnabled := True; }
    FDownhillSimplexAlgorithm.FinalTolerance := iFinalTolerance;
    FDownhillSimplexAlgorithm.ExitDerivative := iExitDerivative;
    FRecreateSimplexFromOriginal := False;
    { Initializing algorithm initial parameters. }
    FAlpha := iAlpha;
    FBeta := iBeta;
    FGamma := iGamma;
    { Saves original point. }
    FOriginalAlpha := iAlpha;
    FOriginalBeta := iBeta;
    FOriginalGamma := iGamma;
    { Initializes auxiliary attributes. }
    FShowAlgoDetails := iShowDetails;
    FInitParamLength := iAlgoInitialStepsAngles;
    FStop := False;
    FRunId := RunId;
end;

destructor TDownHillSimplexHandler.Destroy;
begin
    FDownhillSimplexAlgorithm.Free;
    inherited Destroy;
end;

procedure TDownHillSimplexHandler.StartTimeMeasurement;
begin
    { This supresses useless hints in Lazarus. }
    FPerformanceFrequency := 0;
    FStartTime := 0;
    { Initializing performance counters. }
{$IF NOT DEFINED(FPC)}
    QueryPerformanceFrequency(FPerformanceFrequency);
    QueryPerformanceCounter(FStartTime);
{$ENDIF}
end;

procedure TDownHillSimplexHandler.EndTimeMeasurement;
var
    EndTime: Int64;
begin
    { Calculating computation time. }
    EndTime := 0;
{$IF NOT DEFINED(FPC)}
    QueryPerformanceCounter(EndTime);
{$ENDIF}
    if FPerformanceFrequency <> 0 then
        FComputationTime := (EndTime - FStartTime) / FPerformanceFrequency;
end;

procedure TDownHillSimplexHandler.OptimizeBoundingBox;
var
    Line: string;
begin
    FDownhillSimplexAlgorithm.DownhillSimplexServer := Self;

    StartTimeMeasurement;
    { Optimizing. }
    FDownhillSimplexAlgorithm.AlgorithmRealization;
    EndTimeMeasurement;

    { Gets parameters of best solution. }
    if FShowAlgoDetails then
    begin
        Line := '  Result:' + sLineBreak;
        Line := Line + '     Modified parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [FAlpha, FBeta, FGamma]) +
            sLineBreak;
        Line := Line + '     Volume: ' + Format('%.4f', [FBoxVolume]) + sLineBreak;
        DisplayDetails(Line);
    end;
end;

procedure TDownHillSimplexHandler.DisplayOutput;
begin
    if Assigned(FHandlerOutputProcedure) then
        FHandlerOutputProcedure(Self);
end;

procedure TDownHillSimplexHandler.Stop;
begin
    FStop := True;
end;

function TDownHillSimplexHandler.GetCycleCount: Integer;
begin
    Result := FDownhillSimplexAlgorithm.CycleCount;
end;

function TDownHillSimplexHandler.GetEvaluationCount: Integer;
begin
    Result := FDownhillSimplexAlgorithm.EvaluationCount;
end;

function TDownHillSimplexHandler.GetRestartCount: Integer;
begin
    Result := FDownhillSimplexAlgorithm.RestartCount;
end;

//-----------------------------------------------------------------------------
//--------- TDownHillSimplexHandler - Interface DownHillSimplexServer ---------
//-----------------------------------------------------------------------------

procedure TDownHillSimplexHandler.FillStartDecision(Sender: TComponent;
    iStartDecision: TFloatDecision);
var
    fString: string;
begin
    { Sets up capacity. }
    iStartDecision.ParametersNumber := 3;
    { Simplex is created from original point. }
    if FRecreateSimplexFromOriginal then
    begin
        FAlpha := FOriginalAlpha;
        FBeta := FOriginalBeta;
        FGamma := FOriginalGamma;
    end;
    iStartDecision.Parameters[0] := FAlpha;
    iStartDecision.Parameters[1] := FBeta;
    iStartDecision.Parameters[2] := FGamma;

    if FShowAlgoDetails then
    begin
        fString := '  StartDecision:' + sLineBreak;
        fString := fString + '     Start Parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [FAlpha, FBeta, FGamma]) +
            sLineBreak;
        DisplayDetails(fString);
    end;
end;

{$hints off}
function TDownHillSimplexHandler.GetInitialParameterStep(Sender: TComponent;
    ParameterNumber, ParametersCount: LongInt): Double;
begin
    Result := FInitParamLength;
end;
{$hints on}

procedure TDownHillSimplexHandler.EvaluateDecision(Sender: TComponent;
    iDecision: TFloatDecision);
var
    fString: string;
begin
    { Fills variable parameters from the object. }
    FAlpha := iDecision.Parameters[0];
    FBeta := iDecision.Parameters[1];
    FGamma := iDecision.Parameters[2];
    { Computes evaluation function. }
    FBoxVolume := ComputeRotatedBoxVolume(FAlpha, FBeta, FGamma,
        FBoxMinCoords, FBoxMaxCoords);
    iDecision.Evaluation := FBoxVolume;

    if FShowAlgoDetails then
    begin
        fString := '  EvaluateDecition:' + sLineBreak;
        fString := fString + '     Modified parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [FAlpha, FBeta, FGamma]) +
            sLineBreak;
        fString := fString + '     Volume: ' + Format('%.4f', [iDecision.Evaluation]) +
            sLineBreak;
        DisplayDetails(fString);
    end;
end;

procedure TDownHillSimplexHandler.UpdateResults(Sender: TComponent;
    iDecision: TFloatDecision);
var
    fString: string;
begin
    FAlpha := iDecision.Parameters[0];
    FBeta := iDecision.Parameters[1];
    FGamma := iDecision.Parameters[2];

    if FShowAlgoDetails then
    begin
        fString := 'UpdateResults:' + sLineBreak;
        fString := fString + '    Optimized parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [FAlpha, FBeta, FGamma]) +
            sLineBreak;
        fString := fString + '    Optimized Volume: ' +
            Format('%.4f', [iDecision.Evaluation]) + sLineBreak;
        DisplayDetails(fString);
    end;
end;

function TDownHillSimplexHandler.EndOfCalculation(Sender: TComponent): Boolean;
begin
    { Set up True to interrupt computation. }
    Result := FStop;
end;

end.
