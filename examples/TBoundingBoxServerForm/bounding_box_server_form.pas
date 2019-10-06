unit bounding_box_server_form;

{$mode objfpc}{$H+}

interface

uses
  {$IFNDEF Lazarus}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.Buttons,
  {$ELSE}
    Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
    StdCtrls,
  {$ENDIF}
    Contnrs, Algorithm, DownhillSimplexAlgorithm, Decisions, SimpMath, Math3d;

{$ASSERTIONS ON}
type
    { TBoundingBoxServerForm }
    { Demonstrates the simplest way of integration of algorithm into application.
      The form directly implements IDownhillSimplexServer interface. }
  TBoundingBoxServerForm = class(TForm, IDownhillSimplexServer)
    Button1: TButton;
    CheckBox1: TCheckBox;
    DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
        { Minimum bounding box problem. }
        SavedPointCloud: TComponentList;
        { Set of random points. }
        PointCloud: TComponentList;
        { Angles describing rotation of coordinate system (in degrees). }
        Alpha, Beta, Gamma: Double;
        { Vector displaying position of center of the box. }
        Translation: TDoubleVector3;
        InitialVolume: Double;

        procedure FillParametersFromDecision(Decision: TFloatDecision);
        { TODO: make saving/restoring more efficient. }
        procedure CopyPointCloud(Src: TComponentList; var Dest: TComponentList);
        procedure SavePointCloud;
        procedure RestorePointCloud;

        procedure GenerateRandomPointCloud;
        procedure InitializeVariableParameters;

        { Transforms cloud coordinates according values of variable parameters. }
        procedure TransformPointCloudCoordinates;
        { Executes optimization algorithm. }
        procedure OptimizeVolume;

        { Prints values of variable parameters. }
        procedure PrintParameters(Header: string);
        { Prints data according state of the check box. }
        procedure DisplayPointCloud;
        procedure DisplayParameters;

        { Returns rotation matrix. }
        function GetRotationMatrix: TMatrix;
        { Returns transformation matrix according to current parameters. }
        function GetTransformationMatrix: TMatrix;
        { Computes center of mass of point cloud for initial translation. }
        function ComputeCenterOfMass: TDoubleVector3;
        { Retuns triplet of max coordinates (actually not a vector). }
        function ComputeMaxCoordinates: TDoubleVector3;
        { Retuns triplet of min coordinates (actually not a vector). }
        function ComputeMinCoordinates: TDoubleVector3;
        { Return volume of the box (evaluation function,
          based on values of parameters. }
        function ComputeBoxVolume: Double;

        function DegToRad(Deg: Double): Double;

        { IDownhillSimplexServer }
        //  Return initial characteristic length for every parameter.
        function GetInitParamLength(Sender: TComponent;
            ParameterNumber, ParametersCount: LongInt): Double;

        //  Set inital calculation point in internal representation.
        //  The number of array element is equal to the number of
        //  variable parameters of task being solved.
        procedure FillStartDecision(Sender: TComponent;
            StartDecision: TFloatDecision);
        //  Calculate evaluation function for the point given in internal
        //  representation.
        procedure EvaluateDecision(Sender: TComponent;
            Decision: TFloatDecision);
        //  Displays current minimum.
        procedure UpdateResults(Sender: TComponent; Decision: TFloatDecision);
        //  Return flag of calculation termination.
        function EndOfCalculation(Sender: TComponent): Boolean;
  public
    { public declarations }
  end; 

var
  BoundingBoxServerForm: TBoundingBoxServerForm;

implementation

{$warnings off}
{$hints off}
procedure TBoundingBoxServerForm.GenerateRandomPointCloud;
const
    PointCount: LongInt = 10;     //  Number of points in the cloud.
    //  Dispersion boundaries.
const
    MaxX: double = 0.5;
const
    MinX: double = -0.5;
const
    MaxY: double = 0.5;
const
    MinY: double = -0.5;
const
    MaxZ: double = 0.5;
const
    MinZ: double = -0.5;
    //  Boundaries along (1,1,1) axis.
const
    Max111: double = 10.0;
const
    Min111: double = -10.0;
var
    i: LongInt;
    Point: T3DVector;
    Translation111: double;
begin
    Randomize;
    if PointCloud <> nil then
        PointCloud.Destroy;

    PointCloud := TComponentList.Create(True);

    for i := 0 to PointCount - 1 do
    begin
        Point := T3DVector.Create(nil);

        //  Coordinates are located mainly along (1,1,1) axis
        //  with relatively small dispersion.
        Translation111 := Min111 + Random * (Max111 - Min111);
        Point.Comps[0] := Translation111 + MinX + Random * (MaxX - MinX);
        Point.Comps[1] := Translation111 + MinY + Random * (MaxY - MinY);
        Point.Comps[2] := Translation111 + MinZ + Random * (MaxZ - MinZ);

        PointCloud.Add(Point);
    end;
end;

{$hints on}
{$warnings on}

function TBoundingBoxServerForm.ComputeCenterOfMass: TDoubleVector3;
var
    i: LongInt;
    X, Y, Z: Double;
    Point: T3DVector;
begin
    Assert(PointCloud.Count <> 0);

    X := 0;
    Y := 0;
    Z := 0;
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

procedure TBoundingBoxServerForm.InitializeVariableParameters;
begin
    Translation := ComputeCenterOfMass;
    Alpha := 0;
    Beta := 0;
    Gamma := 0;
    InitialVolume := ComputeBoxVolume;

    Memo1.Lines.Add('Initial volume: ' + FloatToStr(InitialVolume));

    PrintParameters('Initial parameters: ');
end;

function TBoundingBoxServerForm.ComputeMaxCoordinates: TDoubleVector3;
var
    i: LongInt;
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

function TBoundingBoxServerForm.ComputeMinCoordinates: TDoubleVector3;
var
    i: LongInt;
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
function TBoundingBoxServerForm.GetRotationMatrix: TMatrix;
var
    RotX, RotY, RotZ, Matr: TMatrix;
begin
    { Computing rotation matrices.
      Matrices are initalized inside functions. }
    GetMatrixRotX(DegToRad(Alpha), RotX);
    GetMatrixRotY(DegToRad(Beta), RotY);
    GetMatrixRotZ(DegToRad(Gamma), RotZ);
    { Computes rotation matrix. }
    GetUnitMatrix(Matr);
    Mul3DMatrix(RotZ, Matr, Matr);
    Mul3DMatrix(RotY, Matr, Matr);
    Mul3DMatrix(RotX, Matr, Matr);

    Result := Matr;
end;

function TBoundingBoxServerForm.GetTransformationMatrix: TMatrix;
var
    Matr, Rot, Trans, InverseTrans: TMatrix;
    A, B, C: Double;
begin
    Rot := GetRotationMatrix;
    { Computing translation matrices. }
    A := Translation[1];
    B := Translation[2];
    C := Translation[3];

    GetMatrixTrans(A, B, C, Trans);
    GetMatrixTrans(-1.0 * A, -1.0 * B, -1.0 * C, InverseTrans);

    GetUnitMatrix(Matr);
    Mul3DMatrix(InverseTrans, Matr, Matr);
    Mul3DMatrix(Rot, Matr, Matr);
    Mul3DMatrix(Trans, Matr, Matr);

    Result := Matr;
end;

procedure TBoundingBoxServerForm.TransformPointCloudCoordinates;
var
    Matr: TMatrix;
    i: LongInt;
    Point: T3DVector;
    Vector: T3Vector;
begin
    Matr := GetTransformationMatrix;

    for i := 0 to PointCloud.Count - 1 do
    begin
        Point := T3DVector(PointCloud[i]);
        Vector := Point.Vector;
        MulVectMatr(Matr, Vector);
        Point.Vector := Vector;
    end;
end;

{$hints on}

function TBoundingBoxServerForm.ComputeBoxVolume: Double;
var
    MaxCoords, MinCoords: TDoubleVector3;
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

{$warnings off}
{$hints off}
procedure TBoundingBoxServerForm.CopyPointCloud(Src: TComponentList; var Dest: TComponentList);
var
    i: LongInt;
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

{$hints on}
{$warnings on}

procedure TBoundingBoxServerForm.SavePointCloud;
begin
    CopyPointCloud(PointCloud, SavedPointCloud);
end;

procedure TBoundingBoxServerForm.RestorePointCloud;
begin
    CopyPointCloud(SavedPointCloud, PointCloud);
end;

procedure TBoundingBoxServerForm.OptimizeVolume;
begin
    { Initializing algorithm. }
    DownhillSimplexAlgorithm1.ParametersNumber := 6;
    DownhillSimplexAlgorithm1.FinalTolerance := 0.1;
    DownhillSimplexAlgorithm1.RestartDisabled := True;
    DownhillSimplexAlgorithm1.ExitDerivative := 0.5;
    DownhillSimplexAlgorithm1.DownhillSimplexServer := Self;
    { Optimizing. }
    DownhillSimplexAlgorithm1.AlgorithmRealization;
end;

function TBoundingBoxServerForm.GetInitParamLength(Sender: TComponent;
    ParameterNumber, ParametersCount: LongInt): Double;
begin
    Assert(ParametersCount = 6);

    { Sets initial steps for angles 57 degrees, for translations 1 unit. }
    if (ParameterNumber < 3) then
        Result := 57.0
    else
        Result := 1.0;
end;

function TBoundingBoxServerForm.DegToRad(Deg: Double): Double;
begin
    Result := Deg * Pi / 180.0;
end;

//  Set inital calculation point in internal representation.
//  The number of array element is equal to the number of
//  parameters of task to be solved.
procedure TBoundingBoxServerForm.FillStartDecision(Sender: TComponent; StartDecision: TFloatDecision);
begin
    { Sets up capacity. }
    StartDecision.ParametersNumber := 6;
    { Fills variable parameters. }
    StartDecision.Parameters[0] := Alpha;
    StartDecision.Parameters[1] := Beta;
    StartDecision.Parameters[2] := Gamma;
    StartDecision.Parameters[3] := Translation[1];
    StartDecision.Parameters[4] := Translation[2];
    StartDecision.Parameters[5] := Translation[3];
    { Computes evaluation function. }
    StartDecision.Evaluation := ComputeBoxVolume;
end;

procedure TBoundingBoxServerForm.Button1Click(Sender: TObject);
begin
    Memo1.Lines.Clear;
    GenerateRandomPointCloud;
    DisplayPointCloud;
    InitializeVariableParameters;
    OptimizeVolume;
end;

procedure TBoundingBoxServerForm.FillParametersFromDecision(Decision: TFloatDecision);
begin
    Alpha := Decision.Parameters[0];
    Beta := Decision.Parameters[1];
    Gamma := Decision.Parameters[2];
    Translation[1] := Decision.Parameters[3];
    Translation[2] := Decision.Parameters[4];
    Translation[3] := Decision.Parameters[5];
end;

//  Calculate evaluation function for the point given in internal representation.
procedure TBoundingBoxServerForm.EvaluateDecision(Sender: TComponent; Decision: TFloatDecision);

begin
    Assert(Decision.ParametersNumber = 6);

    { Fills variable parameters from the object. }
    FillParametersFromDecision(Decision);
    DisplayParameters;
    SavePointCloud;
    { Transforms coordinates accorging to variable parameters. }
    TransformPointCloudCoordinates;
    DisplayPointCloud;
    { Computes evaluation function. }
    Decision.Evaluation := ComputeBoxVolume;
    RestorePointCloud;
end;

procedure TBoundingBoxServerForm.UpdateResults(Sender: TComponent; Decision: TFloatDecision);
var
    Matr: TMatrix;
    Vector: T3Vector;
begin
    Memo1.Lines.Add('Optimized volume: ' + FloatToStr(Decision.Evaluation));

    FillParametersFromDecision(Decision);
    PrintParameters('Optimized parameters: ');

    { Transforms and prints etalon vector for clearness. }
    Matr := GetRotationMatrix;
    Vector[1] := 1;
    Vector[2] := 0;
    Vector[3] := 0;
    MulVectMatr(Matr, Vector);
    Memo1.Lines.Add('Rotated vector: ');
    Memo1.Lines.Add(
        '  X=' + FloatToStr(Vector[1]) + ', Y=' + FloatToStr(Vector[2]) +
        ', Z=' + FloatToStr(Vector[3])
        );
    Memo1.Lines.Add('');
end;

//  Return flag of termination.
function TBoundingBoxServerForm.EndOfCalculation(Sender: TComponent): Boolean;
begin
    { Set up True to interrupt computation. }
    Result := False;
end;

procedure TBoundingBoxServerForm.DisplayPointCloud;
var
    i: LongInt;
    Point: T3DVector;
begin
    if CheckBox1.Checked then
    begin
        Memo1.Lines.Add('Points:');
        for i := 0 to PointCloud.Count - 1 do
        begin
            Point := T3DVector(PointCloud[i]);
            Memo1.Lines.Add(
                '  X=' + FloatToStr(Point.Comps[0]) + ', Y=' +
                FloatToStr(Point.Comps[1]) + ', Z=' + FloatToStr(Point.Comps[2])
                );
        end;
        Memo1.Lines.Add('');
    end;
end;

procedure TBoundingBoxServerForm.DisplayParameters;
begin
    if CheckBox1.Checked then
        PrintParameters('Modified parameters:');
end;

procedure TBoundingBoxServerForm.PrintParameters(Header: string);
begin
    Memo1.Lines.Add(Header);

    Memo1.Lines.Add('Alpha: ' + FloatToStr(Alpha));
    Memo1.Lines.Add('Beta: ' + FloatToStr(Beta));
    Memo1.Lines.Add('Gamma: ' + FloatToStr(Gamma));

    Memo1.Lines.Add('Translation X: ' + FloatToStr(Translation[1]));
    Memo1.Lines.Add('Translation Y: ' + FloatToStr(Translation[2]));
    Memo1.Lines.Add('Translation Z: ' + FloatToStr(Translation[3]));

    Memo1.Lines.Add('');
end;

initialization
  {$I bounding_box_server_form.lrs}
end.

