unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LazuliButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    LazuliButton1: TLazuliButton;
    LazuliButton2: TLazuliButton;
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

