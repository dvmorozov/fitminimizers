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

        procedure GenerateRandomPointCloud;
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

end.
