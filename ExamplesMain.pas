unit ExamplesMain;

interface

uses
  {$IFNDEF Lazarus}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  {$ELSE}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
    StdCtrls, Contnrs,
  {$ENDIF}
    Algorithm, DownhillSimplexAlgorithm, Decisions, SimpMath, Math3d;

{$ASSERTIONS ON}

type
    { TForm1 }

    TForm1 = class(TForm, IDownhillSimplexServer)
        BitBtn1: TBitBtn;
        DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm;
        Label1: TLabel;
        Memo1: TMemo;
        procedure BitBtn1Click(Sender: TObject);
    private
        { Minimum bounding box problem. }
        SavedPointCloud: TComponentList;
        { Set of random points. }
        PointCloud: TComponentList;
        { Angles describing rotation of coordinate system. }
        Alpha, Beta, Gamma: Double;
        { Vector displaying position of center of the box. }
        BoxPosition:  TDoubleVector3;
        InitialVolume: Double;

        { TODO: saving/restoring make more efficient. }
        procedure CopyPointCloud(Src: TComponentList; var Dest: TComponentList);
        procedure SavePointCloud;
        procedure RestorePointCloud;
        procedure GenerateRandomPointCloud;
        procedure InitializeVariableParameters;
        procedure TransformPointCloudCoordinates;
        procedure OptimizeVolume;
        procedure PrintPointCloud;
        procedure PrintParameters;

        function ComputeCenterOfMass: TDoubleVector3;
        { Retuns triplet of max coordinates (actually not a vector). }
        function ComputeMaxCoordinates: TDoubleVector3;
        { Retuns triplet of min coordinates (actually not a vector). }
        function ComputeMinCoordinates: TDoubleVector3;
        { Return volume of the box, based on values of parameters. }
        function ComputeBoxVolume: Double;

        { IDownhillSimplexServer }
        //  Return initial characteristic length for every parameter.
        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double;

        //  Set inital calculation point in internal representation.
        //  The number of array element is equal to the number of parameters of task to be solved.
        procedure FillStartDecision(Sender: TComponent;
            StartDecision: TFloatDecision);
        //  Calculate evaluation function for the point given in internal representation.
        procedure EvaluateDecision(Sender: TComponent;
            Decision: TFloatDecision);

        procedure UpdateResults(Sender: TComponent;
            Decision: TFloatDecision);
        //  Return flag of calculation termination.
        function EndOfCalculation(Sender: TComponent): Boolean;
    public
        { Public declarations }
    end;

var
    Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
    Memo1.Lines.Clear;
    GenerateRandomPointCloud;
    PrintPointCloud;
    InitializeVariableParameters;
    OptimizeVolume;
end;

procedure TForm1.GenerateRandomPointCloud;
const PointCount: LongInt = 10;     //  Number of points in the cloud.
//  Cloud boundaries.
const MaxX: double = 0.5;
const MinX: double = -0.5;
const MaxY: double = 0.5;
const MinY: double = -0.5;
const MaxZ: double = 0.5;
const MinZ: double = -0.5;
//  Offsets along (1,1,1) axis.
const Max111: double = 10.0;
const Min111: double = -10.0;
var i: LongInt;
    Point: T3DVector;
    Offset111: double;
begin
    Randomize;
    if PointCloud <> nil then
        PointCloud.Destroy;

    PointCloud := TComponentList.Create(True);

    for i := 0 to PointCount - 1 do
    begin
        Point := T3DVector.Create(nil);

        //  Coordinates are located along (1,1,1) axis with
        //  relatively small dispersion.
        Offset111 := Min111 + Random * (Max111 - Min111);
        Point.Comps[0] := Offset111 + MinX + Random * (MaxX - MinX);
        Point.Comps[1] := Offset111 + MinY + Random * (MaxY - MinY);
        Point.Comps[2] := Offset111 + MinZ + Random * (MaxZ - MinZ);

        PointCloud.Add(Point);
    end;
end;

function TForm1.ComputeCenterOfMass: TDoubleVector3;
var i: LongInt;
    X, Y, Z: Double;
    Point: T3DVector;
begin
    Assert(PointCloud.Count <> 0);

    X := 0; Y := 0; Z := 0;
    for i := 0 to PointCloud.Count - 1 do
    begin
        Point := T3DVector(PointCloud[i]);
        X := X + Point.Comps[0];
        Y := Y + Point.Comps[1];
        Z := Z + Point.Comps[2];
    end;

    X := X / PointCloud.Count;
    Y := Y / PointCloud.Count;
    Z := Z / PointCloud.Count;
    Result[1] := X;
    Result[2] := Y;
    Result[3] := Z;
end;

procedure TForm1.InitializeVariableParameters;
begin
    BoxPosition := ComputeCenterOfMass;
    Alpha := 0; Beta := 0; Gamma := 0;
    InitialVolume := ComputeBoxVolume;

    Memo1.Lines.Add('Initial volume: ' + FloatToStr(InitialVolume));
    Memo1.Lines.Add('Initial angles: ');
    Memo1.Lines.Add('Alpha: ' + FloatToStr(Alpha));
    Memo1.Lines.Add('Beta: ' + FloatToStr(Beta));
    Memo1.Lines.Add('Gamma: ' + FloatToStr(Gamma));
    Memo1.Lines.Add('');
end;

function TForm1.ComputeMaxCoordinates: TDoubleVector3;
var i: LongInt;
    Point: T3DVector;
begin
    Assert(PointCloud.Count <> 0);

    Point := T3DVector(PointCloud[0]);
    Result[1] := Point.Comps[0];
    Result[2] := Point.Comps[1];
    Result[3] := Point.Comps[2];

    for i := 1 to PointCloud.Count - 1 do
    begin
        Point := T3DVector(PointCloud[i]);
        if Point.Comps[0] > Result[1] then
            Result[1] := Point.Comps[0];
        if Point.Comps[1] > Result[2] then
            Result[2] := Point.Comps[1];
        if Point.Comps[2] > Result[3] then
            Result[3] := Point.Comps[2];
    end;
end;

function TForm1.ComputeMinCoordinates: TDoubleVector3;
var i: LongInt;
    Point: T3DVector;
begin
    Assert(PointCloud.Count <> 0);

    Point := T3DVector(PointCloud[0]);
    Result[1] := Point.Comps[0];
    Result[2] := Point.Comps[1];
    Result[3] := Point.Comps[2];

    for i := 1 to PointCloud.Count - 1 do
    begin
        Point := T3DVector(PointCloud[i]);
        if Point.Comps[0] < Result[1] then
            Result[1] := Point.Comps[0];
        if Point.Comps[1] < Result[2] then
            Result[2] := Point.Comps[1];
        if Point.Comps[2] < Result[3] then
            Result[3] := Point.Comps[2];
    end;
end;

{$hints off}
procedure TForm1.TransformPointCloudCoordinates;
var RotX, RotY, RotZ, Matr, Trans, InverseTrans: TMatrix;
    i: LongInt;
    Point: T3DVector;
    Vector: T3Vector;
begin
    { Computing rotation matrices. }
    GetMatrixRotX(Alpha, RotX);
    GetMatrixRotY(Beta, RotY);
    GetMatrixRotZ(Gamma, RotZ);
    { Computing translation matrices. }
    GetMatrixTrans(BoxPosition[1], BoxPosition[2], BoxPosition[3], Trans);
    GetMatrixTrans(-1.0 * BoxPosition[1], -1.0 * BoxPosition[2], -1.0 * BoxPosition[3], InverseTrans);
    { Computes rotation matrix. }
    GetUnitMatrix(Matr);
    Mul3DMatrix(InverseTrans, Matr, Matr);
    Mul3DMatrix(RotZ, Matr, Matr);
    Mul3DMatrix(RotY, Matr, Matr);
    Mul3DMatrix(RotX, Matr, Matr);
    Mul3DMatrix(Trans, Matr, Matr);

    for i := 0 to PointCloud.Count - 1 do
    begin
       Point := T3DVector(PointCloud[i]);
       Vector := Point.Vector;
       MulVectMatr(Matr, Vector);
       Point.Vector := Vector;
    end;
end;
{$hints on}

function TForm1.ComputeBoxVolume: Double;
var MaxCoords, MinCoords: TDoubleVector3;
    A, B, C: Double;    //  Sizes of the box.
begin
    { Computes volume of bounding box. }
    MaxCoords := ComputeMaxCoordinates;
    MinCoords := ComputeMinCoordinates;
    A := MaxCoords[1] - MinCoords[1];
    B := MaxCoords[2] - MinCoords[2];
    C := MaxCoords[3] - MinCoords[3];
    Result := A * B * C;
end;

procedure TForm1.CopyPointCloud(Src: TComponentList; var Dest: TComponentList);
var i: LongInt;
    Point: T3DVector;
begin
    if Dest <> nil then
        Dest.Destroy;

    Dest := TComponentList.Create(True);

    for i := 0 to Src.Count - 1 do
    begin
       Point := T3DVector.Create(nil);
       Point.Vector := T3DVector(Src[i]).Vector;
       Dest.Add(Point);
    end;
end;

procedure TForm1.SavePointCloud;
begin
    CopyPointCloud(PointCloud, SavedPointCloud);
end;

procedure TForm1.RestorePointCloud;
begin
    CopyPointCloud(SavedPointCloud, PointCloud);
end;

procedure TForm1.OptimizeVolume;
begin
    { Initializing algorithm. }
    DownhillSimplexAlgorithm1.ParametersNumber := 6;
    //DownhillSimplexAlgorithm1.FinalTolerance := 0.000001;
    DownhillSimplexAlgorithm1.RestartDisabled := True;
    DownhillSimplexAlgorithm1.ExitDerivative := 0.5;
    DownhillSimplexAlgorithm1.DownhillSimplexServer := Self;
    { Optimizing. }
    DownhillSimplexAlgorithm1.AlgorithmRealization;
end;

function TForm1.GetInitParamLength(Sender: TComponent;
    ParameterNumber, ParametersCount: LongInt): Double;
begin
    Result := 0.001;
end;

//  Set inital calculation point in internal representation.
//  The number of array element is equal to the number of parameters
//  of task to be solved.
procedure TForm1.FillStartDecision(Sender: TComponent;
    StartDecision: TFloatDecision);
begin
    { Sets up capacity. }
    StartDecision.ParametersNumber := 6;
    { Fills variable parameters. }
    StartDecision.Parameters[0] := Alpha;
    StartDecision.Parameters[1] := Beta;
    StartDecision.Parameters[2] := Gamma;
    StartDecision.Parameters[3] := BoxPosition[1];
    StartDecision.Parameters[4] := BoxPosition[2];
    StartDecision.Parameters[5] := BoxPosition[3];
    { Computes evaluation function. }
    StartDecision.Evaluation := ComputeBoxVolume;
end;

//  Calculate evaluation function for the point given in internal representation.
procedure TForm1.EvaluateDecision(Sender: TComponent;
    Decision: TFloatDecision);
begin
    Assert(Decision.ParametersNumber = 6);

    { Fills variable parameters from the object. }
    Alpha := Decision.Parameters[0];
    Beta := Decision.Parameters[1];
    Gamma := Decision.Parameters[2];
    BoxPosition[1] := Decision.Parameters[3];
    BoxPosition[2] := Decision.Parameters[4];
    BoxPosition[3] := Decision.Parameters[5];
    PrintParameters;

    SavePointCloud;
    { Transforms coordinates accorging to variable parameters. }
    TransformPointCloudCoordinates;
    PrintPointCloud;
    { Computes evaluation function. }
    Decision.Evaluation := ComputeBoxVolume;
    RestorePointCloud;
end;

procedure TForm1.UpdateResults(Sender: TComponent;
    Decision: TFloatDecision);
begin
    Memo1.Lines.Add('Optimized volume: ' + FloatToStr(Decision.Evaluation));

    Memo1.Lines.Add('Optimized angles: ');
    Memo1.Lines.Add('Alpha: ' + FloatToStr(Decision.Parameters[0]));
    Memo1.Lines.Add('Beta: ' + FloatToStr(Decision.Parameters[1]));
    Memo1.Lines.Add('Gamma: ' + FloatToStr(Decision.Parameters[2]));
    Memo1.Lines.Add('');
end;

//  Return flag of calculation termination.
function TForm1.EndOfCalculation(Sender: TComponent): Boolean;
begin
    { Set up True to interrupt computation. }
    Result := False;
end;

procedure TForm1.PrintPointCloud;
var i: LongInt;
    Point: T3DVector;
begin
    Memo1.Lines.Add('Points:');
    for i := 0 to PointCloud.Count - 1 do
    begin
        Point := T3DVector(PointCloud[i]);
        Memo1.Lines.Add(
            '  X=' + FloatToStr(Point.Comps[0]) +
            ', Y=' + FloatToStr(Point.Comps[1]) +
            ', Z=' + FloatToStr(Point.Comps[2])
            );
    end;
    Memo1.Lines.Add('');
end;

procedure TForm1.PrintParameters;
begin
    Memo1.Lines.Add('Modified parameters: ');

    Memo1.Lines.Add('Alpha: ' + FloatToStr(Alpha));
    Memo1.Lines.Add('Beta: ' + FloatToStr(Beta));
    Memo1.Lines.Add('Gamma: ' + FloatToStr(Gamma));

    Memo1.Lines.Add('Offset X: ' + FloatToStr(BoxPosition[1]));
    Memo1.Lines.Add('Offset Y: ' + FloatToStr(BoxPosition[2]));
    Memo1.Lines.Add('Offset Z: ' + FloatToStr(BoxPosition[3]));

    Memo1.Lines.Add('');
end;

end.
