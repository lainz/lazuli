unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LazuliButton;

type

  { TForm1 }

  TForm1 = class(TForm)
    LazuliButton1: TLazuliButton;
    LazuliButton2: TLazuliButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LazuliButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses Unit1;

{$R *.lfm}

{ TForm1 }

procedure TForm1.LazuliButton1Click(Sender: TObject);
begin
  Unit1.Form2.ShowModal;
  case Unit1.Form2.ModalResult of
     mrOK: LazuliButton1.Caption:='Result OK';
     mrCancel: LazuliButton1.Caption:='Result Cancel';
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Hello');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

