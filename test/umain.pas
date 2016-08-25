unit umain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, ExtCtrls, LazuliButton, LazuliProgressBar;

type

  { TForm1 }

  TForm1 = class(TForm)
    LazuliButton1: TLazuliButton;
    LazuliButton2: TLazuliButton;
    LazuliProgressBar1: TLazuliProgressBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure LazuliButton1Click(Sender: TObject);
    procedure LazuliButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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

procedure TForm1.LazuliButton2Click(Sender: TObject);
begin
  LazuliProgressBar1.AutoSize := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if LazuliProgressBar1.Position = LazuliProgressBar1.Max then
    Timer1.Enabled := False;
  LazuliProgressBar1.Position := LazuliProgressBar1.Position + 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

