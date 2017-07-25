unit ExamplesMain;

{$IFDEF Lazarus}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  DownhillSimplexAlgorithm;

type

  { TForm1 }

  TForm1 = class(TForm)
    DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

