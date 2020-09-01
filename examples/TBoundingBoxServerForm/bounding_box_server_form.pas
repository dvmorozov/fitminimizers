unit bounding_box_server_form;

interface

uses
{$IF NOT DEFINED(FPC)}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.Buttons, System.StrUtils, System.Types,
{$ELSE}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
    StdCtrls, StrUtils,
{$ENDIF}
    SimpMath, Math3d, bounding_box_server,
    int_user_interaction;

{$ASSERTIONS ON}

type
    { TBoundingBoxServerForm }
    { Demonstrates integration of algorithm into application by implementing
      special server interface. }
    TBoundingBoxServerForm = class(TForm, IUserInteraction)
        ComboBoxFiles: TComboBox;
        CheckBoxRandomData: TCheckBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        EditInitialAngleStep: TEdit;
        EditFinalTolerance: TEdit;
        EditExitDerivate: TEdit;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Memo1: TMemo;
        Memo2: TMemo;
        BitBtnFindMinimumBoundingBox: TBitBtn;
        ButtonRandomTest: TButton;
        ButtonBruteForce: TButton;
        ButtonStop: TButton;
        procedure ComboBoxFilesChange(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure BitBtnFindMinimumBoundingBoxClick(Sender: TObject);
        procedure ButtonBruteForceClick(Sender: TObject);
        procedure ButtonStopClick(Sender: TObject);
        procedure ButtonRandomTestClick(Sender: TObject);

    private
        FShowPassed: Boolean;

        procedure PostProcessStatistics;

    public
        function GetInitialAngleStep: Double;
        function GetModelFileName: string;
        function GetFinalTolerance: Double;
        function GetEditExitDerivate: Double;
        { Prints final results among a few runs. }
        procedure DisplayPointCloud(PointCloud: TList);
        { Displays computation results and removes container.
          Should be member of form because works with form controls.
          Removes handler from FHandlers list. }
        procedure DisplayGlobalMinVolume(Handler: TBoundingBoxServer;
            BoxSizes: TDoubleVector3);
        { Displays computation results of single run.
          Should be member of form because works with form controls.
          Removes handler from FHandlers list. }
        procedure DisplayCurrentMinVolume(Handler: TBoundingBoxServer);
        { Displays computation results of single run of brute force search. }
        procedure DisplayBruteForceResult(Handler: TBoundingBoxServer;
            DeltaVolume: Single;
            BoxSizes: TDoubleVector3);
        { TODO: Make ShowDetails private member. }
        procedure DisplayInitialAngles(Alpha, Beta, Gamma: Single; ShowDetails: Boolean);
        procedure DisplayListOfModels(ListOfFiles: TStringList);
        procedure DisplayComputationTime(ComputationTime: TComputationTime);
        procedure DisplayInitialBoxVolume(InitialBoxVolume: Double);
    end;

procedure SortUp(var S1, S2, S3: Double);

var
    BoundingBoxServerForm: TBoundingBoxServerForm;

implementation

uses optimizing_app;

{$R *.dfm}

procedure SortUp(var S1, S2, S3: Double);
var
    Tmp: Double;
begin
    if S2 < S1 then
    begin
        Tmp := S1;
        S1 := S2;
        S2 := Tmp;
    end;
    if S3 < S2 then
    begin
        Tmp := S2;
        S2 := S3;
        S3 := Tmp;
        if S2 < S1 then
        begin
            Tmp := S1;
            S1 := S2;
            S2 := Tmp;
        end;
    end;
end;

function StrToValue(Str: String; var Value: double): Boolean;
var
    Code: Integer;
begin
    Result := True;
    if Pos(',', Str) > 0 then
        Str[Pos(',', Str)] := '.';
    if Pos('°', Str) > 0 then
        Str[Pos('°', Str)] := ' ';
    Str := Trim(Str);
    Val(Str, Value, Code);
    if Code <> 0 then
    begin
        Result := False;
    end;
end;

{ TBoundingBoxServerForm }

function TBoundingBoxServerForm.GetInitialAngleStep: Double;
begin
    { This suppresses useless hints in Lazarus. }
    Result := 37;
    if not StrToValue(EditInitialAngleStep.Text, Result) then
    begin
        Result := 37; //default value
    end;
end;

procedure TBoundingBoxServerForm.FormDestroy(Sender: TObject);
begin
    OptimizingApp.StopComputing;
end;

procedure TBoundingBoxServerForm.ComboBoxFilesChange(Sender: TObject);
begin
    OptimizingApp.ReloadPointCloud := True;
end;

procedure TBoundingBoxServerForm.DisplayCurrentMinVolume(Handler: TBoundingBoxServer);
var
    Matr: TMatrix;
    Vector: T3Vector;
begin
    with Handler do
    begin
        { Displays final angles. }
        Memo1.Lines.Add('Final angles       :' + Format(' %10.4f %10.4f %10.4f',
            [Alpha, Beta, Gamma]));

        { Displays final vector. }
        Matr := GetRotationMatrix(Alpha, Beta, Gamma);
        { Rotates and displays etalon unit vector. }
        Vector[1] := 1; Vector[2] := 0; Vector[3] := 0;
        MulVectMatr(Matr, Vector);
        Memo1.Lines.Add('Final vector       :' +
            Format(' %10.4f %10.4f %10.4f', [Vector[1], Vector[2], Vector[3]]));

        DisplayPointCloud(PointCloud);
    end;
end;

procedure TBoundingBoxServerForm.BitBtnFindMinimumBoundingBoxClick(Sender: TObject);
begin
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;

    OptimizingApp.FindMinimumBoundingBox(CheckBoxRandomData.Checked);
end;

procedure TBoundingBoxServerForm.PostProcessStatistics;
const
    Criterion01 = 0.001;
    // criterion for relative deviation pass/fail; e.g. 0.0 1 => 0.1%
    Criterion1 = 0.01;
    // criterion for relative deviation pass/fail; e.g. 0.01 => 1%

var
    i: Integer;
    Pos, Code: Integer;
    String1, String2, Rate: string;
    Value: Single;
    MinVolume, Deviation, TotalTime, X, Y, Z: Double;
    PassCount01, FailCount01, PassCount1, FailCount1, TotalTimeCount: Integer;
    StringList: TStringList;
    Passed: Boolean;
begin
    MinVolume := 1e20;
    { Gets optimized MinVolume. }
    for i := 0 to Memo2.Lines.Count - 1 do
    begin
        String1 := Trim(Memo2.Lines[i]);
        Pos := PosEx(' ', String1, 1);
        if Pos > 0 then
        begin
            String1 := Trim(Copy(String1, Pos, 1024));
            Pos := PosEx(' ', String1, 1);
            if Pos > 0 then
            begin
                String2 := Trim(Copy(String1, Pos, 1024));
                String1 := Trim(Copy(String1, 1, Pos - 1));
                String1 := StringReplace(String1, ',', '.', [rfReplaceAll]);
                Val(String1, Value, Code);
                if Code = 0 then
                begin
                    if Value < MinVolume then
                    begin
                        MinVolume := Value;
                        Pos := PosEx('(', String2, 1);
                        String2 := Trim(Copy(String2, Pos + 1, 1024));
                        Pos := PosEx(')', String2, 1);
                        String2 := Trim(Copy(String2, 1, Pos - 1));
                        String2 := StringReplace(String2, ',', '.', [rfReplaceAll]);
                        Pos := PosEx(' ', String2, 1);
                        String1 := Trim(Copy(String2, 1, Pos - 1));
                        Val(String1, X, Code);
                        String2 := Trim(Copy(String2, Pos + 1, 1024));
                        Pos := PosEx(' ', String2, 1);
                        String1 := Trim(Copy(String2, 1, Pos - 1));
                        Val(String1, Y, Code);
                        String1 := Trim(Copy(String2, Pos + 1, 1024));
                        Val(String1, Z, Code);
                    end;
                end;
            end;
        end;
    end;
    { Get optimized Volume Pass/Fail statistics. }
    PassCount01 := 0;
    FailCount01 := 0;
    PassCount1 := 0;
    FailCount1 := 0;
    Memo2.Lines.BeginUpdate;
    StringList := TStringList.Create;
    StringList.Duplicates := dupAccept;
    if MinVolume > 0 then
    begin
        for i := 0 to Memo2.Lines.Count - 1 do
        begin
            String1 := Trim(Memo2.Lines[i]);
            Pos := PosEx(' ', String1, 1);
            if Pos > 0 then
            begin
                String1 := Trim(Copy(String1, Pos, 1024));
                Pos := PosEx(' ', String1, 1);
                if Pos > 0 then
                begin
                    String1 := Trim(Copy(String1, 1, Pos - 1));
                    String1 := StringReplace(String1, ',', '.', [rfReplaceAll]);
                    Val(String1, Value, Code);
                    if Code = 0 then
                    begin
                        Deviation := (Value - MinVolume) / MinVolume;
                        Rate := 'Pass';
                        Passed := True;

                        if Deviation < Criterion1 then
                            Inc(PassCount1)
                        else
                        begin
                            Inc(FailCount1);
                            Rate := 'F1';
                            Passed := False;
                        end;

                        if Deviation < Criterion01 then
                            Inc(PassCount01)
                        else
                        begin
                            Inc(FailCount01);
                            Rate := 'F01';
                            Passed := False;
                        end;

                        if not Passed or FShowPassed then
                        begin
                            { Only "failed" tests are added to the resulting list. }
                            StringList.Add(Memo2.Lines[i] + ' - ' + Rate);
                        end;
                    end;
                end;
            end;
        end;
    end;
    Memo2.Clear;
    Memo2.Text := StringList.Text;
    Memo2.Lines.EndUpdate;

    { Get Time to proccess. }
    TotalTime := 0;
    TotalTimeCount := 0;
    for i := 0 to Memo2.Lines.Count - 1 do
    begin
        String1 := Trim(Memo2.Lines[i]);
        Pos := PosEx('---', String1, 1);
        if Pos > 0 then
        begin
            String1 := Trim(Copy(String1, Pos + 3, 1024));
            Pos := PosEx('--', String1, 1);
            if Pos > 0 then
            begin
                String1 := Trim(Copy(String1, 1, Pos - 1));
                String1 := StringReplace(String1, ',', '.', [rfReplaceAll]);
                Val(String1, Value, Code);
                if Code = 0 then
                begin
                    TotalTime := TotalTime + Value;
                    Inc(TotalTimeCount);
                end;
            end;
        end;
    end;
    if TotalTimeCount > 0 then
        TotalTime := TotalTime / TotalTimeCount
    else
        TotalTime := 0;

    if (PassCount01 > 0) then
    begin
        SortUp(X, Y, Z);
        Memo1.Lines.Add('');
        Memo1.Lines.Add('');
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Minimum Volume     : ' +
            Format('%.4f (%6.3f %6.3f %6.3f)', [MinVolume, X, Y, Z]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Deviation := (OptimizingApp.OptiResultBoxVolume - MinVolume) / MinVolume * 100;
        Memo1.Lines.Add('Calculation Delta  : ' +
            Format('%.4f (%.2f%%)', [(OptimizingApp.OptiResultBoxVolume - MinVolume), Deviation]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 0.1%     : ' + Format('%.4f%%',
            [PassCount01 / (PassCount01 + FailCount01) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 1%       : ' + Format('%.4f%%',
            [PassCount1 / (PassCount1 + FailCount1) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Time Average      : ' + Format('%.4f', [TotalTime]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
    end
    else
    begin
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Someting''s was wrong');
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
    end;
end;

procedure TBoundingBoxServerForm.DisplayBruteForceResult(
    Handler: TBoundingBoxServer;
    DeltaVolume: Single;
    BoxSizes: TDoubleVector3);
var
    Line: string;
begin
    { Computes difference in volumes calculated
      for original and rotated orientation. }
    with Handler do
    begin
        DeltaVolume := (BoxVolume - OptimizingApp.GlobalMinVolume);
        Line :=
            Format(
            ' %10.2f %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) -- (%6.2f %6.2f %6.2f) --- %7.4f -- %4d -- %4d -- %2d',
            [DeltaVolume, BoxVolume, BoxSizes[1],
            BoxSizes[2], BoxSizes[3], Alpha, Beta, Gamma,
            PointCloud.Alpha, PointCloud.Beta, PointCloud.Gamma,
            ComputationTime.Time, CycleCount, EvaluationCount, RestartCount]);
        Memo2.Lines.Add(Line);

        Label2.Caption :=
            Format(
            'MinDelta Volume: %8.2f (%6.4f %6.4f %6.4f) ---  MaxDelta Volume: %8.2f (%6.4f %6.4f %6.4f)',
            [OptimizingApp.MinDeltaVolume,
             OptimizingApp.MinBoxSizes[1], OptimizingApp.MinBoxSizes[2], OptimizingApp.MinBoxSizes[3],
             OptimizingApp.MaxDeltaVolume,
             OptimizingApp.MaxBoxSizes[1], OptimizingApp.MaxBoxSizes[2], OptimizingApp.MaxBoxSizes[3]]);
    end;
end;

procedure TBoundingBoxServerForm.ButtonBruteForceClick(Sender: TObject);
begin
    { Adds space. }
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    FShowPassed := True;

    OptimizingApp.BruteForce;
    PostProcessStatistics;
end;

procedure TBoundingBoxServerForm.ButtonRandomTestClick(Sender: TObject);
begin
    { Adds space. }
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;

    OptimizingApp.RandomTest;
    PostProcessStatistics;
end;

procedure TBoundingBoxServerForm.ButtonStopClick(Sender: TObject);
begin
    OptimizingApp.StopComputing;
end;

procedure TBoundingBoxServerForm.DisplayGlobalMinVolume(
    Handler: TBoundingBoxServer; BoxSizes: TDoubleVector3);
var
    Line: string;
begin
    with Handler do
    begin
        Line := Format(
            ' Run %d: %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) --- %7.4f -- %4d -- %4d -- %2d',
            [RunId, BoxVolume, BoxSizes[1], BoxSizes[2], BoxSizes[3],
            Alpha, Beta, Gamma, ComputationTime.Time,
            CycleCount, EvaluationCount,
            RestartCount]);
        Memo1.Lines.Add(Line);
    end;
end;

procedure TBoundingBoxServerForm.DisplayPointCloud(PointCloud: TList);
var
    BoxSizes: TDoubleVector3;
begin
    Memo1.Lines.Add('');
    if CheckBoxRandomData.Checked then
        Memo1.Lines.Add('Random Points')
    else
        Memo1.Lines.Add('File: ' + ComboBoxFiles.Text);
    Memo1.Lines.Add('No of Points: ' + Format(' %10d', [PointCloud.Count]));
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Minimum Volume     :' + Format(' %10.4f', [OptimizingApp.OptiResultBoxVolume]));
    BoxSizes[1] := OptimizingApp.OptiResultBoxMaxCoords[1] - OptimizingApp.OptiResultBoxMinCoords[1];
    BoxSizes[2] := OptimizingApp.OptiResultBoxMaxCoords[2] - OptimizingApp.OptiResultBoxMinCoords[2];
    BoxSizes[3] := OptimizingApp.OptiResultBoxMaxCoords[3] - OptimizingApp.OptiResultBoxMinCoords[3];
    SortUp(BoxSizes[1], BoxSizes[2], BoxSizes[3]);
    Memo1.Lines.Add('Minimum Box        :' + Format(' %10.4f %10.4f %10.4f',
        [BoxSizes[1], BoxSizes[2], BoxSizes[3]]));
end;

procedure TBoundingBoxServerForm.DisplayInitialAngles(Alpha, Beta, Gamma: Single;
    ShowDetails: Boolean);
var
    Matr: TMatrix;
    Vector: T3Vector;
begin
    Matr := GetRotationMatrix(Alpha, Beta, Gamma);

    if ShowDetails then
    begin
        Memo1.Lines.Add('Initial angles     :' +
            Format(' %10.4f %10.4f %10.4f', [Alpha, Beta, Gamma]));

        { Rotates and displays etalon unit vector. }
        Vector[1] := 1; Vector[2] := 0; Vector[3] := 0;
        Matr := GetRotationMatrix(Alpha, Beta, Gamma);
        MulVectMatr(Matr, Vector);
        Memo1.Lines.Add('Initial vector     :' +
            Format(' %10.4f %10.4f %10.4f', [Vector[1], Vector[2], Vector[3]]));
    end;
end;

procedure TBoundingBoxServerForm.DisplayListOfModels(ListOfFiles: TStringList);
var
    i: Integer;
begin
    ComboBoxFiles.Items.Clear;

    for i := 0 to ListOfFiles.Count - 1 do
    begin
        ComboBoxFiles.Items.Add(ListOfFiles[i]);
    end;

    ComboBoxFiles.ItemIndex := 0;
end;

procedure TBoundingBoxServerForm.DisplayComputationTime(ComputationTime: TComputationTime);
begin
    Memo1.Lines.Add('Total Calc Time     : ' + Format(' %.4f', [ComputationTime.Time]));
end;

procedure TBoundingBoxServerForm.DisplayInitialBoxVolume(InitialBoxVolume: Double);
begin
    Memo1.Lines.Add('Initial box volume :' + Format(' %10.4f', [InitialBoxVolume]));
end;

function TBoundingBoxServerForm.GetModelFileName: string;
begin
    Result := ComboBoxFiles.Text;
end;

function TBoundingBoxServerForm.GetFinalTolerance: Double;
begin
    Result := 0.00001;  // default value
    StrToValue(EditFinalTolerance.Text, Result);
end;

function TBoundingBoxServerForm.GetEditExitDerivate: Double;
begin
    Result := 0.5;      // default value
    StrToValue(EditExitDerivate.Text, Result);
end;

end.
