unit bounding_box_server_form;

interface

uses
  {$IFNDEF Lazarus}
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.StdCtrls, Vcl.Buttons, System.StrUtils,
  {$ELSE}
    SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
    StdCtrls, StrUtils, Windows,
  {$ENDIF}
    RunningThread, SimpMath, Math3d, downhill_simplex_handler;

{$ASSERTIONS ON}

type
    p3DVector = ^T3DVector;

    T3DVector = record
        FVector: TDoubleVector3;
    end;

    { TBoundingBoxServerForm }
    { Demonstrates the simplest way of integration of algorithm into application.
      The form directly implements IDownhillSimplexServer interface. }
    TBoundingBoxServerForm = class(TForm)
        ComboBoxFiles: TComboBox;
        CheckBoxRandomData: TCheckBox;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Ed_IniParamLenght: TEdit;
        Ed_FinalTolerance: TEdit;
        Ed_ExitDerivate: TEdit;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Memo1: TMemo;
        Memo2: TMemo;
        BitBtnFindMinimumBoundingBox: TBitBtn;
        ButtonRandomTest: TButton;
        ButtonBruteForce: TButton;
        ButtonStop: TButton;
        function GetIniParamLenght: Double;
        { Prints final results among a few runs. }
        procedure OutputResults;
        procedure BitBtnFindMinimumBoundingBoxClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure PostProcessStatistics;
        procedure ButtonBruteForceClick(Sender: TObject);
        procedure ButtonStopClick(Sender: TObject);
        procedure ButtonRandomTestClick(Sender: TObject);
        function DoOptimizeVolume(iAlpha, iBeta, iGamma: Double;
            iDHS_InitParamLength: Double; var fTime: Single): TDownHillSimplexHandler;
        { Computes minimum box volume starting from a few initial points. }
        function FindMinBoxByVolume: double;
    private
        FilePath: String;

        // optimization results of several algo runs
        OptiResultBoxMinCoords, OptiResultBoxMaxCoords: TDoubleVector3;
        OptiResultBoxVolume: Double;

        ShowAlgoDetails: Boolean;
        ShowPassed: Boolean;
        Stop: Boolean;

        DownHillSimplexHandler: TDownHillSimplexHandler;

        { Executes optimization algorithm. Returns handler instance which
          should be destroyed by calling method. }
        function OptimizeVolume(iAlpha, iBeta, iGamma: Double;
            iDHS_InitParamLength: Double; iShowDetails: Boolean): TDownHillSimplexHandler;

        procedure LoadObjPointCloud(iFileName: String; iAlpha, iBeta, iGamma: single);
        procedure GenerateRandomPointCloud;
        function DegToRad(Deg: Double): Double;
    public
        { Public declarations }
    end;

var
    BoundingBoxServerForm: TBoundingBoxServerForm;
    PointCloud: TList;

implementation

{$R *.dfm}

procedure SortUp(var iS1, iS2, iS3: double);
var
    fTmp: double;
begin
    if iS2 < iS1 then
    begin
        fTmp := iS1;
        iS1 := iS2;
        iS2 := fTmp;
    end;
    if iS3 < iS2 then
    begin
        fTmp := iS2;
        iS2 := iS3;
        iS3 := fTmp;
        if iS2 < iS1 then
        begin
            fTmp := iS1;
            iS1 := iS2;
            iS2 := fTmp;
        end;
    end;
end;

function ConvertValue(iConvStr: String; var iValue: double): Boolean;
var
    fCode: Integer;
begin
    Result := True;
    if Pos(',', iConvStr) > 0 then
        iConvStr[Pos(',', iConvStr)] := '.';
    if Pos('°', iConvStr) > 0 then
        iConvStr[Pos('°', iConvStr)] := ' ';
    iConvStr := Trim(iConvStr);
    Val(iConvStr, iValue, fCode);
    if fCode <> 0 then
    begin
        Result := False;
    end;
end;

{ TBoundingBoxServerForm }

procedure TBoundingBoxServerForm.FormCreate(Sender: TObject);
var
    fSearchResult: TSearchRec;
    fExt: string;
begin
    // reads file list from the directory adjacent to the program directory
    FilePath := ExtractFilePath(ParamStr(0));
    FilePath := IncludeTrailingPathDelimiter(FilePath) + '..' + PathDelim;
    FilePath := ExpandFileName(FilePath) + 'Models' + PathDelim;
    ComboBoxFiles.Items.Clear;
    if FindFirst(FilePath + '*.*', faAnyFile, fSearchResult) = 0 then
    begin
        repeat
            fExt := LowerCase(ExtractFileExt(fSearchResult.Name));
            if (fExt = '.obj') then
            begin
                ComboBoxFiles.Items.Add(fSearchResult.Name);
            end;
        until FindNext(fSearchResult) <> 0;
    end;
    ComboBoxFiles.ItemIndex := 0;
end;

function TBoundingBoxServerForm.OptimizeVolume(iAlpha, iBeta, iGamma: Double;
    iDHS_InitParamLength: Double; iShowDetails: Boolean): TDownHillSimplexHandler;
var
    fFinalTolerance, fExitDerivate: double;
begin
    // this suppresses useless hints in Lazarus
    fFinalTolerance := 0.00001;
    fExitDerivate := 0.5;

    if not ConvertValue(Ed_FinalTolerance.Text, fFinalTolerance) then
    begin
        fFinalTolerance := 0.00001; //default value
    end;
    if not ConvertValue(Ed_ExitDerivate.Text, fExitDerivate) then
    begin
        fExitDerivate := 0.5;       //default value
    end;
    Result := TDownHillSimplexHandler.Create(self);
    with Result do
    begin
        ShowAlgoDetails := iShowDetails;
        SetExitParameters(fFinalTolerance, fExitDerivate);
        OptimizeBoundingBox(iAlpha, iBeta, iGamma, iDHS_InitParamLength);
    end;
end;

function TBoundingBoxServerForm.GetIniParamLenght: Double;
begin
    // this suppresses useless hints in Lazarus
    Result := 37;
    if not ConvertValue(Ed_IniParamLenght.Text, Result) then
    begin
        Result := 37; //default value
    end;
end;

function TBoundingBoxServerForm.DoOptimizeVolume(iAlpha, iBeta, iGamma: Double;
    iDHS_InitParamLength: Double; var fTime: Single): TDownHillSimplexHandler;
var
    fPerformanceFrequency, fStartTime, fEndTime: Int64;
begin
    // this supresses useless hints in Lazarus
    fPerformanceFrequency := 0;
    fStartTime := 0;
    fEndTime := 0;
    fTime := 0;

    QueryPerformanceFrequency(fPerformanceFrequency);
    QueryPerformanceCounter(fStartTime);
    Result := OptimizeVolume(iAlpha, iBeta, iGamma, iDHS_InitParamLength, False);
    QueryPerformanceCounter(fEndTime);

    if fPerformanceFrequency <> 0 then
        fTime := (fEndTime - fStartTime) / fPerformanceFrequency;
end;

procedure TBoundingBoxServerForm.OutputResults;
var
    fDelta: TDoubleVector3;
begin
    Memo1.Lines.Add('');
    if CheckBoxRandomData.Checked then
        Memo1.Lines.Add('Random Points')
    else
        Memo1.Lines.Add('File: ' + ComboBoxFiles.Text);
    Memo1.Lines.Add('No of Points: ' + Format(' %10d', [PointCloud.Count]));
    Memo1.Lines.Add('');
    Memo1.Lines.Add('Minimum Volume    : ' + Format(' %10.4f', [OptiResultBoxVolume]));
    fDelta[1] := OptiResultBoxMaxCoords[1] - OptiResultBoxMinCoords[1];
    fDelta[2] := OptiResultBoxMaxCoords[2] - OptiResultBoxMinCoords[2];
    fDelta[3] := OptiResultBoxMaxCoords[3] - OptiResultBoxMinCoords[3];
    SortUp(fDelta[1], fDelta[2], fDelta[3]);
    Memo1.Lines.Add('Minimum Box       : ' + Format(' %10.4f %10.4f %10.4f',
        [fDelta[1], fDelta[2], fDelta[3]]));
    Application.ProcessMessages;
end;

procedure TBoundingBoxServerForm.BitBtnFindMinimumBoundingBoxClick(Sender: TObject);
var
    FileName: string;
begin
    ShowAlgoDetails := True;
    Stop := False;
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;

    if CheckBoxRandomData.Checked then
    begin
        GenerateRandomPointCloud;
    end
    else
    begin
        //  uses model data
        FileName := FilePath + ComboBoxFiles.Text;
        LoadObjPointCloud(FileName, 0, 45, 45);
    end;
    DownHillSimplexHandler := OptimizeVolume(0, 0, 0, GetIniParamLenght, True);
    DownHillSimplexHandler.Destroy;
    DownHillSimplexHandler := nil;

    OutputResults;
end;

procedure TBoundingBoxServerForm.PostProcessStatistics;
const
    cCriterion01 = 0.001; // criterion for relative deviation pass/fail; e.g. 0.0 1 => 0.1%
    cCriterion1 = 0.01;   // criterion for relative deviation pass/fail; e.g. 0.01 => 1%

var
    x: Integer;
    fP1, fCode: Integer;
    fString, fString2, fRateing: string;
    fValue: Single;
    fMinVolume, fDeviation, fSumTime, fX, fY, fZ: Double;
    fPassCount01, fFailCount01, fPassCount1, fFailCount1, fSumTimeCount: Integer;
    fSL: TStringList;
    Passed: Boolean;
begin
    fMinVolume := 1e20;
    //get optimized MinVolume
    for x := 0 to Memo2.Lines.Count - 1 do
    begin
        fString := Trim(Memo2.Lines[x]);
        fP1 := PosEx(' ', fString, 1);
        if fP1 > 0 then
        begin
            fString := Trim(Copy(fString, fP1, 1024));
            fP1 := PosEx(' ', fString, 1);
            if fP1 > 0 then
            begin
                fString2 := Trim(Copy(fString, fP1, 1024));
                fString := Trim(Copy(fString, 1, fP1 - 1));
                fString := StringReplace(fString, ',', '.', [rfReplaceAll]);
                Val(fString, fValue, fCode);
                if fCode = 0 then
                begin
                    if fValue < fMinVolume then
                    begin
                        fMinVolume := fValue;
                        fP1 := PosEx('(', fString2, 1);
                        fString2 := Trim(Copy(fString2, fP1 + 1, 1024));
                        fP1 := PosEx(')', fString2, 1);
                        fString2 := Trim(Copy(fString2, 1, fP1 - 1));
                        fString2 := StringReplace(fString2, ',', '.', [rfReplaceAll]);
                        fP1 := PosEx(' ', fString2, 1);
                        fString := Trim(Copy(fString2, 1, fP1 - 1));
                        Val(fString, fX, fCode);
                        fString2 := Trim(Copy(fString2, fP1 + 1, 1024));
                        fP1 := PosEx(' ', fString2, 1);
                        fString := Trim(Copy(fString2, 1, fP1 - 1));
                        Val(fString, fY, fCode);
                        fString := Trim(Copy(fString2, fP1 + 1, 1024));
                        Val(fString, fZ, fCode);
                    end;
                end;
            end;
        end;
    end;
    //get optimized Volume Pass/Fail statistics
    fPassCount01 := 0;
    fFailCount01 := 0;
    fPassCount1 := 0;
    fFailCount1 := 0;
    Memo2.Lines.BeginUpdate;
    fSL := TStringList.Create;
    fSL.Duplicates := dupAccept;
    if fMinVolume > 0 then
    begin
        for x := 0 to Memo2.Lines.Count - 1 do
        begin
            fString := Trim(Memo2.Lines[x]);
            fP1 := PosEx(' ', fString, 1);
            if fP1 > 0 then
            begin
                fString := Trim(Copy(fString, fP1, 1024));
                fP1 := PosEx(' ', fString, 1);
                if fP1 > 0 then
                begin
                    fString := Trim(Copy(fString, 1, fP1 - 1));
                    fString := StringReplace(fString, ',', '.', [rfReplaceAll]);
                    Val(fString, fValue, fCode);
                    if fCode = 0 then
                    begin
                        fDeviation := (fValue - fMinVolume) / fMinVolume;
                        fRateing := 'Pass';
                        Passed := True;
                        if fDeviation < cCriterion1 then
                            Inc(fPassCount1)
                        else
                        begin
                            Inc(fFailCount1);
                            fRateing := 'F1';
                            Passed := False;
                        end;
                        if fDeviation < cCriterion01 then
                            Inc(fPassCount01)
                        else
                        begin
                            Inc(fFailCount01);
                            fRateing := 'F01';
                            Passed := False;
                        end;
                        if not Passed or ShowPassed then
                        begin
                            // Only "failed" tests are added to the resulting list.
                            fSL.Add(Memo2.Lines[x] + ' - ' + fRateing);
                        end;
                    end;
                end;
            end;
        end;
    end;
    Memo2.Clear;
    Memo2.Text := fSL.Text;
    Memo2.Lines.EndUpdate;

    // get Time to proccess
    fSumTime := 0;
    fSumTimeCount := 0;
    for x := 0 to Memo2.Lines.Count - 1 do
    begin
        fString := Trim(Memo2.Lines[x]);
        fP1 := PosEx('---', fString, 1);
        if fP1 > 0 then
        begin
            fString := Trim(Copy(fString, fP1 + 3, 1024));
            fP1 := PosEx('--', fString, 1);
            if fP1 > 0 then
            begin
                fString := Trim(Copy(fString, 1, fP1 - 1));
                fString := StringReplace(fString, ',', '.', [rfReplaceAll]);
                Val(fString, fValue, fCode);
                if fCode = 0 then
                begin
                    fSumTime := fSumTime + fValue;
                    Inc(fSumTimeCount);
                end;
            end;
        end;
    end;
    if fSumTimeCount > 0 then
        fSumTime := fSumTime / fSumTimeCount
    else
        fSumTime := 0;

    if (fPassCount01 > 0) then
    begin
        SortUp(fX, fY, fZ);
        Memo1.Lines.Add('');
        Memo1.Lines.Add('');
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Minimum Volume    : ' +
            Format('%.4f (%6.3f %6.3f %6.3f)', [fMinVolume, fX, fY, fZ]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        fDeviation := (OptiResultBoxVolume - fMinVolume) / fMinVolume * 100;
        Memo1.Lines.Add('Calculation Delta : ' + Format('%.4f (%.2f%%)', [(OptiResultBoxVolume - fMinVolume), fDeviation]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 0.1%     : ' + Format('%.4f%%',
            [fPassCount01 / (fPassCount01 + fFailCount01) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Passrate 1%       : ' + Format('%.4f%%',
            [fPassCount1 / (fPassCount1 + fFailCount1) * 100]));
        Memo1.Lines.Add(
            '----------------------------------------------------------------------------');
        Memo1.Lines.Add('Time Average      : ' + Format('%.4f', [fSumTime]));
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

procedure TBoundingBoxServerForm.ButtonBruteForceClick(Sender: TObject);
const
    cSteps = 2;
var
    x, y, z: Integer;
    FileName, fResult: string;
    fAlpha, fBeta, fGamma: Single;
    fMaxDeltaVolume, fMinDeltaVolume, fDeltaVolume, fTime: Single;
    fBoxVolume: Double;
    fMinDeltaCord, fMaxDeltaCord, fDeltaCord: TDoubleVector3;
begin
    FileName := FilePath + ComboBoxFiles.Text;

    ShowAlgoDetails := False;
    ShowPassed := True;
    Stop := False;
    fMaxDeltaVolume := -1.0e20;
    fMinDeltaVolume := 1.0e20;
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    Application.ProcessMessages;

    // get the optimized volume and Box size
    // load it in original orientation
    LoadObjPointCloud(FileName, 0, 0, 0);
    fBoxVolume := FindMinBoxByVolume;

    // do the test for brute force orientation
    for x := 0 to (179 div cSteps) do
        for y := 0 to (179 div cSteps) do
            for z := 0 to (179 div cSteps) do
            begin
                if not Stop then
                begin
                    fAlpha := x * cSteps;
                    fBeta := y * cSteps;
                    fGamma := z * cSteps;

                    LoadObjPointCloud(FileName, fAlpha, fBeta, fGamma);
                    fTime := 0;
                    DownHillSimplexHandler := DoOptimizeVolume(0, 0, 0, GetIniParamLenght, fTime);
                    if not Stop then
                    begin
                        //  Computes difference in volumes calculated
                        //  for original and rotated orientation.
                        with DownHillSimplexHandler do
                        begin
                            fDeltaVolume := (BoxVolume - fBoxVolume);
                            //  Computes lengths of edges of bounding box.
                            fDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                            fDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                            fDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                            //  Sorts edges.
                            SortUp(fDeltaCord[1], fDeltaCord[2], fDeltaCord[3]);
                            fResult := Format(
                                ' %10.2f %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) -- (%6.2f %6.2f %6.2f) --- %7.4f -- %4d -- %4d -- %2d',
                                [fDeltaVolume, BoxVolume, fDeltaCord[1], fDeltaCord[2],
                                fDeltaCord[3], Alpha, Beta, Gamma,
                                fAlpha, fBeta, fGamma, fTime,
                                DHS_CycleCount, DHS_EvaluationCount, DHS_RestartCount]);
                            if fDeltaVolume > fMaxDeltaVolume then
                            begin
                                fMaxDeltaVolume := fDeltaVolume;
                                fMaxDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                                fMaxDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                                fMaxDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                                SortUp(fMaxDeltaCord[1], fMaxDeltaCord[2], fMaxDeltaCord[3]);
                            end;
                            if fDeltaVolume < fMinDeltaVolume then
                            begin
                                fMinDeltaVolume := fDeltaVolume;
                                fMinDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                                fMinDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                                fMinDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                                SortUp(fMinDeltaCord[1], fMinDeltaCord[2], fMinDeltaCord[3]);
                            end;
                            Memo2.Lines.Add(fResult);
                            Label2.Caption :=
                                Format('MinDelta Volume: %8.2f (%6.4f %6.4f %6.4f) ---  MaxDelta Volume: %8.2f (%6.4f %6.4f %6.4f)', [fMinDeltaVolume, fMinDeltaCord[1], fMinDeltaCord[2],
                                fMinDeltaCord[3], fMaxDeltaVolume, fMaxDeltaCord[1], fMaxDeltaCord[2],
                                fMaxDeltaCord[3]]);
                        end;
                    end;
                    DownHillSimplexHandler.Destroy;
                    DownHillSimplexHandler := nil;
                    Application.ProcessMessages;
                end;
            end;
    PostProcessStatistics;
end;

procedure TBoundingBoxServerForm.ButtonRandomTestClick(Sender: TObject);
var
    x: Integer;
    FileName, fResult: string;
    fAlpha, fBeta, fGamma: Single;
    fMinDeltaVolume, fMaxDeltaVolume, fDeltaVolume, fTime: Single;
    fBoxVolume: Double;
    fMinDeltaCord, fMaxDeltaCord, fDeltaCord: TDoubleVector3;
begin
    FileName := FilePath + ComboBoxFiles.Text;

    ShowAlgoDetails := False;
    Stop := False;
    fMaxDeltaVolume := -1.0e20;
    fMinDeltaVolume := 1.0e20;
    Memo1.Lines.Clear;
    Memo2.Lines.Clear;
    Application.ProcessMessages;

    // get the optimized volume and Box size
    // load it in original orientation
    LoadObjPointCloud(FileName, 0, 0, 0);
    fBoxVolume := FindMinBoxByVolume;

    // do the test for random orientation
    Randomize;
    for x := 0 to 99999 do
    begin
        if not Stop then
        begin
            fAlpha := Random * 180;
            fBeta := Random * 180;
            fGamma := Random * 180;
            LoadObjPointCloud(FileName, fAlpha, fBeta, fGamma);

            fTime := 0;
            DownHillSimplexHandler := DoOptimizeVolume(0, 0, 0, GetIniParamLenght, fTime);
            if not Stop then
            begin
                with DownHillSimplexHandler do
                begin
                    fDeltaVolume := (BoxVolume - fBoxVolume);
                    fDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                    fDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                    fDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                    SortUp(fDeltaCord[1], fDeltaCord[2], fDeltaCord[3]);
                    fResult := Format(
                        ' %10.2f %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) -- (%6.2f %6.2f %6.2f) --- %7.4f -- %4d -- %4d -- %2d',
                        [fDeltaVolume, BoxVolume, fDeltaCord[1], fDeltaCord[2],
                        fDeltaCord[3], Alpha, Beta, Gamma,
                        fAlpha, fBeta, fGamma, fTime,
                        DHS_CycleCount, DHS_EvaluationCount, DHS_RestartCount]);
                    if fDeltaVolume > fMaxDeltaVolume then
                    begin
                        fMaxDeltaVolume := fDeltaVolume;
                        fMaxDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                        fMaxDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                        fMaxDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                        SortUp(fMaxDeltaCord[1], fMaxDeltaCord[2], fMaxDeltaCord[3]);
                    end;
                    if fDeltaVolume < fMinDeltaVolume then
                    begin
                        fMinDeltaVolume := fDeltaVolume;
                        fMinDeltaCord[1] := BoxMaxCoords[1] - BoxMinCoords[1];
                        fMinDeltaCord[2] := BoxMaxCoords[2] - BoxMinCoords[2];
                        fMinDeltaCord[3] := BoxMaxCoords[3] - BoxMinCoords[3];
                        SortUp(fMinDeltaCord[1], fMinDeltaCord[2], fMinDeltaCord[3]);
                    end;
                    Memo2.Lines.Add(fResult);
                    Label2.Caption := Format(
                        'MinDelta Volume: %8.2f (%6.4f %6.4f %6.4f) ---  MaxDelta Volume: %8.2f (%6.4f %6.4f %6.4f)',
                        [fMinDeltaVolume, fMinDeltaCord[1], fMinDeltaCord[2],
                        fMinDeltaCord[3], fMaxDeltaVolume, fMaxDeltaCord[1], fMaxDeltaCord[2],
                        fMaxDeltaCord[3]]);
                end;
            end;
            DownHillSimplexHandler.Destroy;
            DownHillSimplexHandler := nil;
            Application.ProcessMessages;
        end;
    end;
    PostProcessStatistics;
end;

procedure TBoundingBoxServerForm.ButtonStopClick(Sender: TObject);
begin
    Stop := True;
    if assigned(DownHillSimplexHandler) then
        DownHillSimplexHandler.Stop;
end;

function TBoundingBoxServerForm.FindMinBoxByVolume: Double;
const cStartAngle5Runs: array[0..4] of TDoubleVector3 = (
    (0, 0, 0),
    (45, 45, 45),
    (-45, -45, -45),
    (45, -45, 45),
    (-45, 45, -45)
    );
const cStartAngle9Runs: array[0..8] of TDoubleVector3 = (
    (0, 0, 0),
    (45, 45, 45),
    (-45, -45, -45),
    (45, -45, 45),
    (-45, 45, -45),
    (-45, 45, 45),
    (-45, -45, 45),
    (45, -45, -45),
    (45, 45, -45)
    );var
    i: integer;
    fRuns: integer;
    fStartAngle, fBoxSize: TDoubleVector3;
    fBoxVolume: Double;
    fTime: Single;
    fResult: string;
    iMinCoords, iMaxCoords: TDoubleVector3;
begin
    fRuns:= 3;
    if PointCloud.Count < 100000 then fRuns:= 5;
    if PointCloud.Count < 25000 then fRuns:= 9;
    fBoxVolume:= 1e30;
    for i:= 0 to fRuns-1 do begin
      if not Stop then begin
        if fRuns <= 5 then fStartAngle:= cStartAngle5Runs[i]
        else fStartAngle:= cStartAngle9Runs[i];

        fTime := 0;
        // Optimization Run to get the minimum Volume
        DownHillSimplexHandler := DoOptimizeVolume(
            fStartAngle[1], fStartAngle[2], fStartAngle[3], GetIniParamLenght, fTime);
        with DownHillSimplexHandler do
        begin
            if BoxVolume < fBoxVolume then
            begin
                fBoxVolume := BoxVolume;
                iMaxCoords := BoxMaxCoords;
                iMinCoords := BoxMinCoords;
            end;
            fBoxSize[1]:= BoxMaxCoords[1] - BoxMinCoords[1];
            fBoxSize[2]:= BoxMaxCoords[2] - BoxMinCoords[2];
            fBoxSize[3]:= BoxMaxCoords[3] - BoxMinCoords[3];
            SortUp(fBoxSize[1], fBoxSize[2], fBoxSize[3]);
            fResult := Format(
                ' Run %d: %10.2f (%6.3f %6.3f %6.3f) -- (%7.2f %7.2f %7.2f) --- %7.4f -- %4d -- %4d -- %2d',
                [i+1, BoxVolume, fBoxSize[1], fBoxSize[2], fBoxSize[3], Alpha, Beta,
                Gamma, fTime, DHS_CycleCount, DHS_EvaluationCount, DHS_RestartCount]);
            Memo1.Lines.Add(fResult);
            Application.ProcessMessages;
        end;
        DownHillSimplexHandler.Destroy;
        DownHillSimplexHandler := nil;
      end;
    end;

    Result := fBoxVolume;

    OptiResultBoxVolume := fBoxVolume;
    OptiResultBoxMaxCoords := iMaxCoords;
    OptiResultBoxMinCoords := iMinCoords;
    OutputResults;
end;

procedure TBoundingBoxServerForm.LoadObjPointCloud(iFileName: string;
    iAlpha, iBeta, iGamma: Single);
type
    TOBJCoord = record // Stores X, Y, Z coordinates
        X, Y, Z: Single;
    end;

    function GetCoords(iString: string): TOBJCoord;
    var
        P, P2, P3: Integer;
        fCoord: TOBJCoord;
    begin
        iString := Trim(Copy(iString, 3, Length(iString)));
        P := Pos(' ', iString);
        P2 := PosEx(' ', iString, P + 1);
        P3 := PosEx(' ', iString, P2 + 1);
        if P3 = 0 then
            P3 := 1000;
        iString := StringReplace(iString, '.', FormatSettings.DecimalSeparator,
            [rfReplaceAll]);
        fCoord.X := StrToFloat(Copy(iString, 1, P - 1));
        fCoord.Y := StrToFloat(Copy(iString, P + 1, P2 - P - 1));
        fCoord.Z := StrToFloat(Copy(iString, P2 + 1, P3 - P2 - 1));
        Result := fCoord;
    end;

var
    x: Integer;
    F: TextFile;
    S: string;
    fCoord: TOBJCoord;
    fPoint: p3DVector;
    RotX, RotY, RotZ, Matr: TMatrix;
    fVector: T3Vector;
begin
    if PointCloud <> nil then
    begin
        for x := 0 to PointCloud.Count - 1 do
        begin
            fPoint := PointCloud[x];
            Dispose(fPoint);
        end;
        PointCloud.Free;
        PointCloud := nil;
    end;
    PointCloud := TList.Create;
    if FileExists(iFileName) then
    begin
        RotX := MatrixRotX(DegToRad(iAlpha));
        RotY := MatrixRotY(DegToRad(iBeta));
        RotZ := MatrixRotZ(DegToRad(iGamma));
        { Computes rotation matrix. }
        Matr := UnitMatrix;
        Mul3DMatrix(RotZ, Matr, Matr);
        Mul3DMatrix(RotY, Matr, Matr);
        Mul3DMatrix(RotX, Matr, Matr);

        fVector[1] := 1;
        fVector[2] := 0;
        fVector[3] := 0;
        MulVectMatr(Matr, fVector);

        AssignFile(F, iFileName);
        Reset(F);
        while not (EOF(F)) do
        begin
            Application.ProcessMessages;
            Readln(F, S);
            if (Length(S) >= 2) and (S[1] <> '#') then
            begin
                S := Uppercase(S);
                if (S[1] = 'V') and (S[2] = ' ') then
                begin
                    // Read Vertex Data
                    New(fPoint);
                    fCoord := GetCoords(S);
                    fVector[1] := fCoord.X;
                    fVector[2] := fCoord.Y;
                    fVector[3] := fCoord.Z;
                    MulVectMatr(Matr, fVector);

                    fPoint^.FVector := fVector;
                    PointCloud.Add(fPoint);
                end;
            end;
        end;
        CloseFile(F);
    end;
end;

function TBoundingBoxServerForm.DegToRad(Deg: Double): Double;
begin
    Result := Deg * Pi / 180.0;
end;

{$warnings off}
{$hints off}
procedure TBoundingBoxServerForm.GenerateRandomPointCloud;
const
    PointCount: LongInt = 10;     //  Number of points in the cloud.
    //  Dispersion boundaries.
    MaxX: double = 0.5;
    MinX: double = -0.5;
    MaxY: double = 0.5;
    MinY: double = -0.5;
    MaxZ: double = 0.5;
    MinZ: double = -0.5;
    //  Boundaries along (1,1,1) axis.
    Max111: double = 10.0;
    Min111: double = -10.0;
var
    i, x: LongInt;
    Point: p3DVector;
    Translation111: double;
begin
    Randomize;
    if PointCloud <> nil then
    begin
        for x := 0 to PointCloud.Count - 1 do
        begin
            Point := PointCloud[x];
            Dispose(Point);
        end;
        PointCloud.Free;
        PointCloud := nil;
    end;

    PointCloud := TList.Create;

    for i := 0 to PointCount - 1 do
    begin
        new(Point);
        //  Coordinates are located mainly along (1,1,1) axis
        //  with relatively small dispersion.
        Translation111 := Min111 + Random * (Max111 - Min111);
        Point^.FVector[1] := Translation111 + MinX + Random * (MaxX - MinX);
        Point^.FVector[2] := Translation111 + MinY + Random * (MaxY - MinY);
        Point^.FVector[3] := Translation111 + MinZ + Random * (MaxZ - MinZ);

        PointCloud.Add(Point);
    end;
end;

{$hints on}
{$warnings on}

end.
