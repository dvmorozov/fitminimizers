unit int_user_interaction;

interface

uses
    Classes, SysUtils, SimpMath, bounding_box_server;

type
    IUserInteraction = interface
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
        procedure DisplayInitialAngles(Alpha, Beta, Gamma: Single; ShowDetails: Boolean);
        procedure DisplayListOfModels(ListOfFiles: TStringList);
        procedure DisplayComputationTime(ComputationTime: TComputationTime);
        procedure DisplayInitialBoxVolume(InitialBoxVolume: Double);
    end;

implementation

end.

