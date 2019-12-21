unit downhill_simplex_handler;

interface

uses
{$IFNDEF Lazarus}
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
    P3DVector = ^T3DVector;

    T3DVector = record
        FVector: TDoubleVector3;
    end;

    TPointCloud = class(TList)
    private
        { Data rotation angles relative to original position. }
        FAlpha, FBeta, FGamma: Single;
    public
        constructor Create(AAlpha, ABeta, AGamma: Single); reintroduce;

        property Alpha: Single read FAlpha;
        property Beta: Single read FBeta;
        property Gamma: Single read FGamma;
    end;

    TComputationTime = class
    private
        FPerformanceFrequency, FStartTime: Int64;
        FTime: Single;

    public
        procedure StartMeasurement;
        procedure EndMeasurement;

        property Time: Single read FTime;
    end;

    TDownHillSimplexHandler = class;
    { External method displaying attributes of container instance. }
    THandlerOutputProcedure = procedure(Handler: TDownHillSimplexHandler) of object;

    TDownHillSimplexHandler = class(TComponent, IDownhillSimplexServer)
    private
        FPointCloud: TPointCloud;
        { FPointCloud can be shared between a few instances of "handler".
          In that case it should be released by caller. }
        FOwnsPointCloud: Boolean;

        { Minimum bounding box problem. }
        FDownhillSimplexAlgorithm: TDownhillSimplexAlgorithm;
        FHandlerOutputProcedure: THandlerOutputProcedure;
        { Measures time of optimization. }
        FComputationTime: TComputationTime;

        FStop: Boolean;
        FShowDetails: Boolean;
        FInitialAngleStep: Double;
        FRecreateSimplexFromOriginal: Boolean;

        { Optimized values of angles describing rotation of coordinate system (in degrees). }
        FAlpha, FBeta, FGamma: Double;
        { Original values of angles describing rotation of coordinate system (in degrees). }
        FOriginalAlpha, FOriginalBeta, FOriginalGamma: Double;

        { Box infomations: volume and coordinates}
        FBoxVolume: Double;
        { Vectors containing triplets of maximum and minimum coordinates of
          model points. They are used to compute bounding box volume. }
        FBoxMinCoords, FBoxMaxCoords: TDoubleVector3;
        { Unique container id. It is used only to reference results. }
        FRunId: Integer;

        function GetCycleCount: Integer;
        function GetEvaluationCount: Integer;
        function GetRestartCount: Integer;

        procedure ClearPointCloud;

        function ComputeRotatedBoxVolume(AAlpha, ABeta, AGamma: Single;
            var AMinCoords, AMaxCoords: T3Vector): Double;

        { IDownhillSimplexServer implementation. }

        { Returns initial characteristic length for every parameter. }
        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double;
        { Sets inital calculation point in internal representation.
          The number of array element is equal to the number of
          variable parameters of task being solved. }
        procedure FillStartDecision(Sender: TComponent; StartDecision: TFloatDecision);
        { Calculates evaluation function for the point given in internal
          representation. }
        procedure EvaluateDecision(Sender: TComponent; Decision: TFloatDecision);
        { Displays current minimum. }
        procedure UpdateResults(Sender: TComponent; Decision: TFloatDecision);
        { Returns flag of calculation termination. }
        function EndOfCalculation(Sender: TComponent): Boolean;

    public
        { If set simplex is recreated from original point on restarting,
          otherwise from the best point found during last optimization cycle.
          Handler doesn't copy point cloud, it either owns it or just keep
          reference to external object. }
        constructor Create(AOwner: TComponent; AAlpha, ABeta, AGamma,
            AInitialAngleStep: Double;
            AFinalTolerance, AExitDerivative: Double;
            AShowDetails: Boolean; ARunId: Integer;
            APointCloud: TPointCloud; AOwnsPointCloud: Boolean); reintroduce;
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
        property ComputationTime: TComputationTime read FComputationTime;
        property RunId: Integer read FRunId;
        property PointCloud: TPointCloud read FPointCloud;
    end;

implementation

uses bounding_box_server_form;

procedure TComputationTime.StartMeasurement;
begin
    { Initializing performance counters. }
    FPerformanceFrequency := 0;
    FStartTime := 0;
{$IFNDEF Lazarus}
    QueryPerformanceFrequency(FPerformanceFrequency);
    QueryPerformanceCounter(FStartTime);
{$ENDIF}
end;

procedure TComputationTime.EndMeasurement;
{$IFNDEF Lazarus}
var
    EndTime: Int64;
{$ENDIF}
begin
    FTime := 0;
{$IFNDEF Lazarus}
    EndTime := 0;
    QueryPerformanceCounter(EndTime);
    if FPerformanceFrequency <> 0 then
        FTime := (EndTime - FStartTime) / FPerformanceFrequency;
{$ENDIF}
end;

constructor TPointCloud.Create(AAlpha, ABeta, AGamma: Single);
begin
    inherited Create;
    FAlpha := AAlpha;
    FBeta := ABeta;
    FGamma := AGamma;
end;

function TDownHillSimplexHandler.ComputeRotatedBoxVolume(AAlpha, ABeta, AGamma: Single;
    var AMinCoords, AMaxCoords: T3Vector): Double;

var
    i: Integer;
    A, B, C: Double; //  Sizes of the box.
    Matr: TMatrix;
    Point: P3DVector;
    Vector: T3Vector;
begin
    { Computes volume of bounding box. }
    Matr := GetRotationMatrix(AAlpha, ABeta, AGamma);
    Point := FPointCloud[0];
    Vector := Point^.fVector;
    MulVectMatr(Matr, Vector);
    AMaxCoords := Vector;
    AMinCoords := Vector;
    for i := 1 to FPointCloud.Count - 1 do
    begin
        Point := FPointCloud[i];
        Vector := Point^.fVector;
        MulVectMatr(Matr, Vector);
        if Vector[1] > AMaxCoords[1] then
            AMaxCoords[1] := Vector[1];
        if Vector[2] > AMaxCoords[2] then
            AMaxCoords[2] := Vector[2];
        if Vector[3] > AMaxCoords[3] then
            AMaxCoords[3] := Vector[3];
        if Vector[1] < AMinCoords[1] then
            AMinCoords[1] := Vector[1];
        if Vector[2] < AMinCoords[2] then
            AMinCoords[2] := Vector[2];
        if Vector[3] < AMinCoords[3] then
            AMinCoords[3] := Vector[3];
    end;
    A := AMaxCoords[1] - AMinCoords[1];
    B := AMaxCoords[2] - AMinCoords[2];
    C := AMaxCoords[3] - AMinCoords[3];
    Result := A * B * C;
end;

procedure DisplayDetails(iString: string);
begin
    BoundingBoxServerForm.Memo2.Lines.Add(iString);
end;

//-----------------------------------------------------------------------------
//-------------------------- TDownHillSimplexHandler --------------------------
//-----------------------------------------------------------------------------

constructor TDownHillSimplexHandler.Create(
    AOwner: TComponent; AAlpha, ABeta, AGamma,
    AInitialAngleStep: Double;
    AFinalTolerance, AExitDerivative: Double;
    AShowDetails: Boolean; ARunId: Integer;
    APointCloud: TPointCloud; AOwnsPointCloud: Boolean);
begin
    inherited Create(AOwner);
    FPointCloud := APointCloud;
    FOwnsPointCloud := AOwnsPointCloud;

    FDownhillSimplexAlgorithm := TDownhillSimplexAlgorithm.Create(Self);
    { Initializing algorithm exit parameters. }
    FDownhillSimplexAlgorithm.FinalTolerance := 0.00001;
    FDownhillSimplexAlgorithm.ExitDerivative := 0.5;
    FDownhillSimplexAlgorithm.RestartDisabled := False;
    { FDownhillSimplexAlgorithm.SimplexDirectionChangingEnabled := True; }
    FDownhillSimplexAlgorithm.FinalTolerance := AFinalTolerance;
    FDownhillSimplexAlgorithm.ExitDerivative := AExitDerivative;
    FRecreateSimplexFromOriginal := False;
    { Initializing algorithm initial parameters. }
    FAlpha := AAlpha;
    FBeta := ABeta;
    FGamma := AGamma;
    { Saves original point. }
    FOriginalAlpha := AAlpha;
    FOriginalBeta := ABeta;
    FOriginalGamma := AGamma;
    { Initializes auxiliary attributes. }
    FShowDetails := AShowDetails;
    FInitialAngleStep := AInitialAngleStep;
    FStop := False;
    FRunId := ARunId;
    FComputationTime := TComputationTime.Create;
end;

destructor TDownHillSimplexHandler.Destroy;
begin
    ClearPointCloud;
    FDownhillSimplexAlgorithm.Free;
    FComputationTime.Free;
    inherited Destroy;
end;

procedure TDownHillSimplexHandler.OptimizeBoundingBox;
var
    Line: string;
begin
    FDownhillSimplexAlgorithm.DownhillSimplexServer := Self;

    FComputationTime.StartMeasurement;
    { Optimizing. }
    FDownhillSimplexAlgorithm.AlgorithmRealization;
    FComputationTime.EndMeasurement;

    { Gets parameters of best solution. }
    if FShowDetails then
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

procedure TDownHillSimplexHandler.ClearPointCloud;
var
    Point: P3DVector;
    x: Integer;
begin
    if FOwnsPointCloud then
    begin
        if FPointCloud <> nil then
        begin
            for x := 0 to FPointCloud.Count - 1 do
            begin
                Point := FPointCloud[x];
                Dispose(Point);
            end;
            FPointCloud.Free;
            FPointCloud := nil;
        end;
    end;
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
    StartDecision: TFloatDecision);
var
    Line: string;
begin
    { Sets up capacity. }
    StartDecision.ParametersNumber := 3;
    { Simplex is created from original point. }
    if FRecreateSimplexFromOriginal then
    begin
        FAlpha := FOriginalAlpha;
        FBeta := FOriginalBeta;
        FGamma := FOriginalGamma;
    end;
    StartDecision.Parameters[0] := FAlpha;
    StartDecision.Parameters[1] := FBeta;
    StartDecision.Parameters[2] := FGamma;

    if FShowDetails then
    begin
        Line := '  StartDecision:' + sLineBreak;
        Line := Line + '     Start Parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [FAlpha, FBeta, FGamma]) +
            sLineBreak;
        DisplayDetails(Line);
    end;
end;

{$hints off}
function TDownHillSimplexHandler.GetInitParamLength(Sender: TComponent;
    ParameterNumber, ParametersCount: LongInt): Double;
begin
    Result := FInitialAngleStep;
end;
{$hints on}

procedure TDownHillSimplexHandler.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
var
    Line: string;
begin
    { Fills variable parameters from the object. }
    FAlpha := Decision.Parameters[0];
    FBeta := Decision.Parameters[1];
    FGamma := Decision.Parameters[2];
    { Computes evaluation function. }
    FBoxVolume := ComputeRotatedBoxVolume(FAlpha, FBeta, FGamma,
        FBoxMinCoords, FBoxMaxCoords);
    Decision.Evaluation := FBoxVolume;

    if FShowDetails then
    begin
        Line := '  EvaluateDecition:' + sLineBreak;
        Line := Line + '     Modified parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [FAlpha, FBeta, FGamma]) +
            sLineBreak;
        Line := Line + '     Volume: ' + Format('%.4f', [Decision.Evaluation]) +
            sLineBreak;
        DisplayDetails(Line);
    end;
end;

procedure TDownHillSimplexHandler.UpdateResults(Sender: TComponent;
    Decision: TFloatDecision);
var
    Line: string;
begin
    FAlpha := Decision.Parameters[0];
    FBeta := Decision.Parameters[1];
    FGamma := Decision.Parameters[2];

    if FShowDetails then
    begin
        Line := 'UpdateResults:' + sLineBreak;
        Line := Line + '    Optimized parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [FAlpha, FBeta, FGamma]) +
            sLineBreak;
        Line := Line + '    Optimized Volume: ' +
            Format('%.4f', [Decision.Evaluation]) + sLineBreak;
        DisplayDetails(Line);
    end;
end;

function TDownHillSimplexHandler.EndOfCalculation(Sender: TComponent): Boolean;
begin
    { Set up True to interrupt computation. }
    Result := FStop;
end;

end.
