unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LazuliButton;

type

  { TForm2 }

  TForm2 = class(TForm)
    LazuliButton3: TLazuliButton;
    LazuliButton4: TLazuliButton;
    procedure FormShow(Sender: TObject);
    procedure LazuliButton3Click(Sender: TObject);
    procedure LazuliButton4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.LazuliButton3Click(Sender: TObject);
begin

end;

procedure TForm2.FormShow(Sender: TObject);
begin
  Self.ActiveControl := LazuliButton3;
end;

procedure TForm2.LazuliButton4Click(Sender: TObject);
begin

end;

end.

