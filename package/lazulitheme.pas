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
    procedure DrawButton(Control: TLazuliButton; State: TLazuliButtonStates;
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
  State: TLazuliButtonStates; Bitmap: TBGRABitmap);
var
  ts: TSize;
  Width, Height: integer;
begin
  AssignFont(Control, Bitmap);
  ts := Bitmap.TextSize(Control.Caption);
  Width := Control.Width;
  Height := Control.Height;

  if Control.Enabled then
  begin
    if lbsActive in State then
    begin
      { Button Down }
      Bitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
        BGRA(61, 61, 61), dmSet);
      Bitmap.Rectangle(1, 1, Width - 1, Height - 2, BGRA(55, 55, 55),
        BGRA(61, 61, 61), dmSet);
      Bitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
    end
    else
    begin
      if lbsHovered in State then
      begin
        { Button Hovered }
        Bitmap.GradientFill(0, 0, Width, Height, BGRA(132, 132, 132),
          BGRA(109, 109, 109), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
        Bitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(1, 1, Width - 2, BGRA(160, 160, 160));
        Bitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
      end
      else
      begin
        { Button Normal }
        Bitmap.GradientFill(0, 0, Width, Height, BGRA(107, 107, 107),
          BGRA(84, 84, 84), gtLinear, PointF(0, 0), PointF(0, Height), dmSet);
        Bitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48), dmSet);
        Bitmap.SetHorizLine(1, 1, Width - 2, BGRA(130, 130, 130));
        Bitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
        { Button Focused }
        if Control.Focused then
        begin
          Bitmap.Rectangle(1, 2, Width - 1, Height - 2, BGRA(80, 111, 172), dmSet);
        end;
      end;
    end;
  end
  else
  begin
    { Button Disabled }
    Bitmap.Rectangle(0, 0, Width, Height - 1, BGRA(48, 48, 48),
      BGRA(61, 61, 61), dmSet);
    Bitmap.SetHorizLine(0, Height - 1, Width - 1, BGRA(115, 115, 115));
  end;

  if Control.Enabled then
  begin
    Bitmap.TextOut((Width - ts.cx) div 2, ((Height - ts.cy) div 2) -
      1, Control.Caption, BGRA(47, 47, 47));
    Bitmap.TextOut((Width - ts.cx) div 2, (Height - ts.cy) div 2,
      Control.Caption, BGRA(229, 229, 229));
  end
  else
    Bitmap.TextOut((Width - ts.cx) div 2, (Height - ts.cy) div 2,
      Control.Caption, BGRA(170, 170, 170));
end;

initialization
  LazuliStyle := TLazuliTheme.Create;

finalization
  FreeAndNil(LazuliStyle);

end.
