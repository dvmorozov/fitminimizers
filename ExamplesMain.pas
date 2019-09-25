unit ExamplesMain;

interface

uses
  {$IFNDEF Lazarus}
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  {$ELSE}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  {$ENDIF}
  Algorithm, DownhillSimplexAlgorithm;

type
  TForm1 = class(TForm)
    DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
