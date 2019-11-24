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
    StdCtrls, Windows,
{$ENDIF}
    Algorithm, DownhillSimplexAlgorithm, Decisions, SimpMath, Math3d;

type
    TDownHillSimplexHandler = class(TComponent, IDownhillSimplexServer)
    private
        { Minimum bounding box problem. }
        FDownhillSimplexAlgorithm: TDownhillSimplexAlgorithm;

        gStop: Boolean;
        gShowAlgoDetails: Boolean;

        { Optimized values of angles describing rotation of coordinate system (in degrees). }
        gAlpha, gBeta, gGamma: Double;
        { Original values of angles describing rotation of coordinate system (in degrees). }
        gOriginalAlpha, gOriginalBeta, gOriginalGamma: Double;
        FRecreateSimplexFromOriginal: Boolean;

        gDHS_InitParamLength: Double;

        { Box infomations: volume and coordinates}
        gBoxVolume: Double;
        { Vectors containing triplets of maximum and minimum coordinates of
          model points. They are used to compute bounding box volume. }
        gBoxMinCoords, gBoxMaxCoords: TDoubleVector3;
        { Computation time. }
        FComputationTime: Single;

        function Get_DHS_CycleCount: Integer;
        function Get_DHS_EvaluationCount: Integer;
        function Get_DHS_RestartCount: Integer;

        { IDownhillSimplexServer }
        //  Return initial characteristic length for every parameter.
        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double;
        //  Set inital calculation point in internal representation.
        //  The number of array element is equal to the number of
        //  variable parameters of task being solved.
        procedure FillStartDecision(Sender: TComponent; iStartDecision: TFloatDecision);
        //  Calculate evaluation function for the point given in internal
        //  representation.
        procedure EvaluateDecision(Sender: TComponent; iDecision: TFloatDecision);
        //  Displays current minimum.
        procedure UpdateResults(Sender: TComponent; iDecision: TFloatDecision);
        //  Return flag of calculation termination.
        function EndOfCalculation(Sender: TComponent): Boolean;

    public
        { If set simplex is recreated from original point on restarting,
          otherwise from the best point found during last optimization cycle. }
        constructor Create(AOwner: TComponent; iAlpha, iBeta, iGamma,
            iAlgoInitialStepsAngles: Double;
            iFinalTolerance, iExitDerivative: Double;
            iShowDetails: Boolean); reintroduce;
        destructor Destroy; override;
        { Initializes performance counters and starts optimization.
          The procedure should not have parameters because it is called
          from separate thread. }
        procedure OptimizeBoundingBox;
        { Interrupts computing. }
        procedure Stop;
        { Optimized values of angles describing rotation of coordinate system (in degrees). }
        property Alpha: Double read gAlpha;
        property Beta: Double read gBeta;
        property Gamma: Double read gGamma;
        { Box infomations: volume and coordinates. }
        property BoxVolume: Double read gBoxVolume;
        property BoxMinCoords: TDoubleVector3 read gBoxMinCoords;
        property BoxMaxCoords: TDoubleVector3 read gBoxMaxCoords;
        { DownHillSimplex algorithm statistical details. }
        property DHS_CycleCount: Integer read Get_DHS_CycleCount;
        property DHS_EvaluationCount: Integer read Get_DHS_EvaluationCount;
        property DHS_RestartCount: Integer read Get_DHS_RestartCount;

        property RecreateSimplexFromOriginal: Boolean
            read FRecreateSimplexFromOriginal write FRecreateSimplexFromOriginal;
        property ComputationTime: Single read FComputationTime;
    end;


implementation

uses bounding_box_server_form;

function ComputeRotatedBoxVolume(iAlpha, iBeta, iGamma: Single;
    var iMinCoords, iMaxCoords: T3Vector): Double;

    function GetRotationMatrix(iAlpha, iBeta, iGamma: Single): TMatrix;

        function DegToRad(iDeg: Double): Double;
        begin
            Result := iDeg * PI / 180.0;
        end;

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
    Application.ProcessMessages;
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
    BoundingBoxServerForm.Memo2.Lines.Add(iString);
end;

//-----------------------------------------------------------------------------
//-------------------------- TDownHillSimplexHandler --------------------------
//-----------------------------------------------------------------------------

constructor TDownHillSimplexHandler.Create(
    AOwner: TComponent; iAlpha, iBeta, iGamma,
    iAlgoInitialStepsAngles: Double;
    iFinalTolerance, iExitDerivative: Double;
    iShowDetails: Boolean);
begin
    inherited Create(AOwner);
    FDownhillSimplexAlgorithm := TDownhillSimplexAlgorithm.Create(Self);
    { Initializing algorithm - Exit Parameter }
    FDownhillSimplexAlgorithm.FinalTolerance := 0.00001;
    FDownhillSimplexAlgorithm.ExitDerivative := 0.5;
    FDownhillSimplexAlgorithm.RestartDisabled := False;
    //FDownhillSimplexAlgorithm.SimplexDirectionChangingEnabled:= True;
    FDownhillSimplexAlgorithm.FinalTolerance := iFinalTolerance;
    FDownhillSimplexAlgorithm.ExitDerivative := iExitDerivative;

    { Initializing algorithm - Start Parameter }
    gAlpha := iAlpha;
    gBeta := iBeta;
    gGamma := iGamma;
    { Saves original point. }
    gOriginalAlpha := iAlpha;
    gOriginalBeta := iBeta;
    gOriginalGamma := iGamma;

    gShowAlgoDetails := iShowDetails;
    gDHS_InitParamLength := iAlgoInitialStepsAngles;
    gStop := False;

    FRecreateSimplexFromOriginal := False;
end;

destructor TDownHillSimplexHandler.Destroy;
begin
    FDownhillSimplexAlgorithm.Free;
    inherited Destroy;
end;

procedure TDownHillSimplexHandler.OptimizeBoundingBox;
var
    fString: string;
    fPerformanceFrequency, fStartTime, fEndTime: Int64;
begin
    // this supresses useless hints in Lazarus
    fPerformanceFrequency := 0;
    fStartTime := 0;
    fEndTime := 0;
    FComputationTime := 0;

    FDownhillSimplexAlgorithm.DownhillSimplexServer := Self;

    { Initializing performance counters. }
    QueryPerformanceFrequency(fPerformanceFrequency);
    QueryPerformanceCounter(fStartTime);
    { Optimizing. }
    FDownhillSimplexAlgorithm.AlgorithmRealization;
    { Calculating computation time. }
    QueryPerformanceCounter(fEndTime);
    if fPerformanceFrequency <> 0 then
        FComputationTime := (fEndTime - fStartTime) / fPerformanceFrequency;

    { Gets parameters of best solution. }
    if gShowAlgoDetails then
    begin
        fString := '  Result:' + sLineBreak;
        fString := fString + '     Modified parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [gAlpha, gBeta, gGamma]) +
            sLineBreak;
        fString := fString + '     Volume: ' + Format('%.4f', [gBoxVolume]) + sLineBreak;
        DisplayDetails(fString);
    end;
end;

procedure TDownHillSimplexHandler.Stop;
begin
    gStop := True;
end;

function TDownHillSimplexHandler.Get_DHS_CycleCount: Integer;
begin
    Result := FDownhillSimplexAlgorithm.CycleCount;
end;

function TDownHillSimplexHandler.Get_DHS_EvaluationCount: Integer;
begin
    Result := FDownhillSimplexAlgorithm.EvaluationCount;
end;

function TDownHillSimplexHandler.Get_DHS_RestartCount: Integer;
begin
    Result := FDownhillSimplexAlgorithm.RestartCount;
end;

//-----------------------------------------------------------------------------
//--------- TDownHillSimplexHandler - Interface DownHillSimplexServer ---------
//-----------------------------------------------------------------------------

//  Set inital calculation point in internal representation.
//  The number of array element is equal to the number of
//  parameters of task to be solved.

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
        gAlpha := gOriginalAlpha;
        gBeta := gOriginalBeta;
        gGamma := gOriginalGamma;
    end;
    iStartDecision.Parameters[0] := gAlpha;
    iStartDecision.Parameters[1] := gBeta;
    iStartDecision.Parameters[2] := gGamma;

    if gShowAlgoDetails then
    begin
        fString := '  StartDecision:' + sLineBreak;
        fString := fString + '     Start Parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [gAlpha, gBeta, gGamma]) +
            sLineBreak;
        DisplayDetails(fString);
    end;
end;

{$hints off}
function TDownHillSimplexHandler.GetInitParamLength(Sender: TComponent;
    ParameterNumber, ParametersCount: LongInt): Double;
begin
    Result := gDHS_InitParamLength;
end;

{$hints on}

procedure TDownHillSimplexHandler.EvaluateDecision(Sender: TComponent;
    iDecision: TFloatDecision);
var
    fString: string;
begin
    { Fills variable parameters from the object. }
    gAlpha := iDecision.Parameters[0];
    gBeta := iDecision.Parameters[1];
    gGamma := iDecision.Parameters[2];
    { Computes evaluation function. }
    gBoxVolume := ComputeRotatedBoxVolume(gAlpha, gBeta, gGamma,
        gBoxMinCoords, gBoxMaxCoords);
    iDecision.Evaluation := gBoxVolume;

    if gShowAlgoDetails then
    begin
        fString := '  EvaluateDecition:' + sLineBreak;
        fString := fString + '     Modified parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [gAlpha, gBeta, gGamma]) +
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
    gAlpha := iDecision.Parameters[0];
    gBeta := iDecision.Parameters[1];
    gGamma := iDecision.Parameters[2];

    if gShowAlgoDetails then
    begin
        fString := 'UpdateResults:' + sLineBreak;
        fString := fString + '    Optimized parameters:' +
            Format('Alpha: %.4f Beta: %.4f Gamma: %.4f', [gAlpha, gBeta, gGamma]) +
            sLineBreak;
        fString := fString + '    Optimized Volume: ' +
            Format('%.4f', [iDecision.Evaluation]) + sLineBreak;
        DisplayDetails(fString);
    end;
end;

//  Return flag of termination.

function TDownHillSimplexHandler.EndOfCalculation(Sender: TComponent): Boolean;
begin
    { Set up True to interrupt computation. }
    Result := gStop;
end;

end.
