unit ExamplesMain;

interface

uses
  {$IFNDEF Lazarus}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  {$ELSE}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
    Contnrs,
  {$ENDIF}
    Algorithm, DownhillSimplexAlgorithm, SimpMath;

type
    { TForm1 }

    TForm1 = class(TForm)
        BitBtn1: TBitBtn;
        DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm;
        procedure BitBtn1Click(Sender: TObject);
    private
        { Private declarations }
        { Minimum bounding box problem. }
        { Set of random points. }
        PointCloud: TComponentList;
        { Unit vector displaying box orientation. }
        BoxOrientation: TDoubleVector3;
        { Vector displaying position of center of the box. }
        BoxPosition:  TDoubleVector3;
        { Sizes of the box. }
        A, B, C: Double;

        procedure GenerateRandomPointCloud;
        procedure InitializeVariableParameters;

        function ComputeCenterOfMass: TDoubleVector3;
        { Retuns triplet of max coordinates (actually not a vector). }
        function ComputeMaxCoordinates: TDoubleVector3;
        { Retuns triplet of min coordinates (actually not a vector). }
        function ComputeMinCoordinates: TDoubleVector3;
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
    GenerateRandomPointCloud;
end;

procedure TForm1.GenerateRandomPointCloud;
const PointCount: LongInt = 10;     //  Number of points in the cloud.
//  Cloud boundaries.
const MaxX: double = 10.0;
const MinX: double = -10.0;
const MaxY: double = 10.0;
const MinY: double = -10.0;
const MaxZ: double = 10.0;
const MinZ: double = -10.0;
var i: LongInt;
    Point: T3DVector;
begin
    Randomize;
    if PointCloud <> nil then
        PointCloud.Destroy;

    PointCloud := TComponentList.Create(True);

    for i := 0 to PointCount - 1 do
    begin
        Point := T3DVector.Create(nil);
        Point.Comps[0] := MinX + Random * (MaxX - MinX);
        Point.Comps[1] := MinY + Random * (MaxY - MinY);
        Point.Comps[2] := MinZ + Random * (MaxZ - MinZ);

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
var MaxCoords, MinCoords: TDoubleVector3;
begin
    BoxPosition := ComputeCenterOfMass;
    BoxOrientation[1] := 1; BoxOrientation[2] := 0; BoxOrientation[3] := 0;
    MaxCoords := ComputeMaxCoordinates;
    MinCoords := ComputeMinCoordinates;
    A := MaxCoords[1] - MinCoords[1];
    B := MaxCoords[2] - MinCoords[2];
    C := MaxCoords[3] - MinCoords[3];
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

end.
