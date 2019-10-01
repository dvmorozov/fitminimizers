unit ExamplesMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DownhillSimplexAlgorithm; 

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    DownhillSimplexAlgorithm1: TDownhillSimplexAlgorithm;
    Label1: TLabel;
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;

implementation

initialization
  {$I ExamplesMain.lrs}

end.

