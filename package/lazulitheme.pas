unit LazuliTheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, LazuliButton, LazuliProgressBar;

type

  { TLazuliTheme }

  TLazuliTheme = class
  private
    { ProgressBar}
    FRandSeed: integer;
  protected
    procedure AssignFont(Control: TCustomControl; Bitmap: TBGRABitmap);
  public
    constructor Create;
    { Button }
    procedure MeasureButton(Control: TLazuliButton; Bitmap: TBGRABitmap;
      var PreferredWidth, PreferredHeight: integer);
    procedure DrawButton(Control: TLazuliButton; State: TLazuliButtonStates;
      Bitmap: TBGRABitmap);
    { ProgressBar }
    procedure MeasureProgressBar(Control: TLazuliProgressBar; Bitmap: TBGRABitmap;
      var PreferredWidth, PreferredHeight: integer);
    procedure DrawProgressBar(Control: TLazuliProgressBar; Bitmap: TBGRABitmap);
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

constructor TLazuliTheme.Create;
begin
  randomize;
  FRandSeed := randseed;
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

procedure TLazuliTheme.MeasureProgressBar(Control: TLazuliProgressBar;
  Bitmap: TBGRABitmap; var PreferredWidth, PreferredHeight: integer);
begin
  PreferredWidth := 75;
  PreferredHeight := 25;
end;

procedure TLazuliTheme.DrawProgressBar(Control: TLazuliProgressBar;
  Bitmap: TBGRABitmap);

  function ApplyLightness(c: TBGRAPixel; lightness: word): TBGRAPixel;
  begin
    Result := GammaCompression(SetLightness(GammaExpansion(c), lightness));
  end;

  procedure DrawBar(bounds: TRect);
  var
    lCol: TBGRAPixel;
  begin
    lCol := ColorToBGRA(ColorToRGB(Control.Color));

    DoubleGradientAlphaFill(Bitmap, bounds,
      ApplyLightness(lCol, 37000), ApplyLightness(lCol, 29000),
      ApplyLightness(lCol, 26000), ApplyLightness(lCol, 18000),
      gdVertical, gdVertical, gdVertical, 0.53);

    InflateRect(bounds, -1, -1);

    DoubleGradientAlphaFill(Bitmap, bounds,
      ApplyLightness(lCol, 28000), ApplyLightness(lCol, 22000),
      ApplyLightness(lCol, 19000), ApplyLightness(lCol, 11000),
      gdVertical, gdVertical, gdVertical, 0.53);
  end;

var
  content: TRect;
  xpos, y, tx, ty: integer;
  grayValue: integer;
begin
  tx := Control.ClientWidth;
  ty := Control.ClientHeight;
  Bitmap.Fill(BGRA(83, 83, 83));
  Bitmap.Rectangle(0, 0, tx, ty, BGRA(255, 255, 255, 6), dmDrawWithTransparency);
  if (tx > 2) and (ty > 2) then
    Bitmap.Rectangle(1, 1, tx - 1, ty - 1, BGRA(29, 29, 29), dmSet);

  if (tx > 4) and (ty > 4) then
  begin
    content  := Rect(2, 2, tx - 2, ty - 2);
    randseed := FRandSeed;
    for y := content.Top to content.Bottom - 1 do
    begin
      if y = content.Top then
        grayValue := 33
      else
      if y = content.Top + 1 then
        grayValue := 43
      else
        grayValue := 47 + random(50 - 47 + 1);
      Bitmap.SetHorizLine(content.Left, y, content.Right - 1, BGRA(
        grayValue, grayValue, grayValue));
    end;
    if tx >= 6 then
      Bitmap.DrawVertLine(content.Right - 1, content.Top, content.Bottom - 1,
        BGRA(0, 0, 0, 32));
    if Control.Max > Control.Min then
    begin
      xpos := round((Control.Position - Control.Min) / (Control.Max - Control.Min) *
        (content.right - content.left)) + content.left;
      if xpos > content.left then
      begin
        DrawBar(rect(content.left, content.top, xpos, content.bottom));
        if xpos < content.right then
        begin
          Bitmap.SetPixel(xpos, content.top, BGRA(62, 62, 62));
          Bitmap.SetVertLine(xpos, content.top + 1, content.bottom - 1, BGRA(40, 40, 40));
        end;
      end;
    end;
  end;
end;

initialization
  LazuliStyle := TLazuliTheme.Create;

finalization
  FreeAndNil(LazuliStyle);

end.
