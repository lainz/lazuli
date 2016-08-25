unit LazuliTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, BGRABitmap, BGRABitmapTypes,
  LazuliButton;

type

  { TLazuliTheme }

  TLazuliTheme = class
  protected
    procedure AssignFont(Control: TCustomControl; Bitmap: TBGRABitmap);
  public
    { Button }
    procedure MeasureButton(Control: TLazuliButton; Bitmap: TBGRABitmap;
      var PreferredWidth, PreferredHeight: integer);
    procedure DrawButton(Control: TLazuliButton; State: TLazuliButtonState;
      Bitmap: TBGRABitmap);
  end;

var
  LazuliStyle: TLazuliTheme;

implementation

{ TLazuliTheme }

procedure TLazuliTheme.AssignFont(Control: TCustomControl; Bitmap: TBGRABitmap);
begin
  Bitmap.FontName := Control.Font.Name;
  Bitmap.FontStyle := Control.Font.Style;
  Bitmap.FontHeight := Control.Font.Height;
  case Control.Font.Quality of
    fqDefault: Bitmap.FontQuality := fqSystemClearType;
    fqDraft: Bitmap.FontQuality := fqSystemClearType;
    fqProof: Bitmap.FontQuality := fqSystemClearType;
    fqNonAntialiased: Bitmap.FontQuality := fqSystemClearType;
    fqAntialiased: Bitmap.FontQuality := fqFineAntialiasing;
    fqCleartype: Bitmap.FontQuality := fqSystemClearType;
    fqCleartypeNatural: Bitmap.FontQuality := fqSystemClearType;
  end;
end;

procedure TLazuliTheme.MeasureButton(Control: TLazuliButton;
  Bitmap: TBGRABitmap; var PreferredWidth, PreferredHeight: integer);
var
  ts: TSize;
begin
  AssignFont(Control, Bitmap);
  ts := Bitmap.TextSize(Control.Caption);
  Inc(PreferredWidth, ts.cx + 26);
  Inc(PreferredHeight, ts.cy + 10);
end;

procedure TLazuliTheme.DrawButton(Control: TLazuliButton;
  State: TLazuliButtonState; Bitmap: TBGRABitmap);
var
  ts: TSize;
begin
  AssignFont(Control, Bitmap);
  ts := Bitmap.TextSize(Control.Caption);
  if Control.Enabled then
  begin
    case State of
      lbsNormal: Bitmap.FillRoundRectAntialias(0, 0, Control.Width,
          Control.Height, 10, 10, BGRAWhite, [], False);
      lbsHovered: Bitmap.FillRoundRectAntialias(0, 0, Control.Width,
          Control.Height, 10, 10, BGRA(200, 200, 200), [], False);
      lbsActive: Bitmap.FillRoundRectAntialias(0, 0, Control.Width,
          Control.Height, 10, 10, BGRA(100, 100, 100), [], False);
    end;
    if Control.Focused then
      Bitmap.RoundRectAntialias(2, 2, Control.Width-3, Control.Height-3, 10, 10, BGRA(230, 230, 230), 2);
    Bitmap.TextOut((Control.Width - ts.cx) div 2, (Control.Height - ts.cy) div
      2, Control.Caption, BGRABlack);
  end
  else
  begin
    Bitmap.FillRoundRectAntialias(0, 0, Control.Width, Control.Height,
      10, 10, BGRA(75, 75, 75), [], False);
    Bitmap.TextOut((Control.Width - ts.cx) div 2, (Control.Height - ts.cy) div
      2, Control.Caption, BGRA(200, 200, 200));
  end;
end;

initialization
  LazuliStyle := TLazuliTheme.Create;

finalization
  FreeAndNil(LazuliStyle);

end.
