unit LazuliProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Types,
  ComCtrls, Dialogs, BGRABitmap;

type

  { TLazuliProgressBar }

  TLazuliProgressBar = class(TCustomControl)
  private
    FBarShowText: boolean;
    FBorderWidth: integer;
    FMaxValue: integer;
    FMinValue: integer;
    FOrientation: TProgressBarOrientation;
    FSmooth: boolean;
    FStep: integer;
    FStyle: TProgressBarStyle;
    FValue: integer;
    FBGRA: TBGRABitmap;
    procedure SetFBarShowText(AValue: boolean);
    procedure SetFBorderWidth(AValue: integer);
    procedure SetFOrientation(AValue: TProgressBarOrientation);
    procedure SetFSmooth(AValue: boolean);
    procedure SetFStep(AValue: integer);
    procedure SetFStyle(AValue: TProgressBarStyle);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetValue(const AValue: integer);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    procedure Paint; override;
    procedure PrepareBuffer;
    procedure DrawBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property BarShowText: boolean read FBarShowText write SetFBarShowText;
    property BorderSpacing;
    property BorderWidth: integer read FBorderWidth write SetFBorderWidth;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Orientation: TProgressBarOrientation read FOrientation write SetFOrientation;
    property PopupMenu;
    property Min: integer read FMinValue write SetMinValue;
    property Max: integer read FMaxValue write SetMaxValue;
    property Position: integer read FValue write SetValue;
    property Smooth: boolean read FSmooth write SetFSmooth;
    property Step: integer read FStep write SetFStep;
    property Style: TProgressBarStyle read FStyle write SetFStyle;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property Color;
  published
    property Hint;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

procedure Register;

implementation

uses BGRABitmapTypes, LazuliTheme;

procedure Register;
begin
  RegisterComponents('Lazuli', [TLazuliProgressBar]);
end;

{ TLazuliProgressBar }

procedure TLazuliProgressBar.SetMinValue(const AValue: integer);
begin
  if FMinValue = AValue then
    exit;
  FMinValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FMaxValue < FMinValue then
    FMaxValue := FMinValue;
  Invalidate;
end;

procedure TLazuliProgressBar.SetValue(const AValue: integer);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  if FValue < FMinValue then
    FValue := FMinValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  Invalidate;
end;

class function TLazuliProgressBar.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 25;
end;

procedure TLazuliProgressBar.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  LazuliStyle.MeasureProgressBar(Self, FBGRA, PreferredWidth, PreferredHeight);
end;

procedure TLazuliProgressBar.Paint;
begin
  PrepareBuffer;
  LazuliStyle.DrawProgressBar(Self, FBGRA);
  DrawBuffer;
end;

procedure TLazuliProgressBar.PrepareBuffer;
begin
  if (FBGRA.Width <> Width) or (FBGRA.Height <> Height) then
    FBGRA.SetSize(Width, Height);

  FBGRA.FillTransparent;
end;

procedure TLazuliProgressBar.DrawBuffer;
begin
  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TLazuliProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 30;
  FBGRA := TBGRABitmap.Create(Width, Height);
  Color := BGRAToColor(BGRA(102, 163, 226));
end;

destructor TLazuliProgressBar.Destroy;
begin
  FreeAndNil(FBGRA);
  inherited Destroy;
end;

procedure TLazuliProgressBar.SetMaxValue(const AValue: integer);
begin
  if FMaxValue = AValue then
    exit;
  FMaxValue := AValue;
  if FValue > FMaxValue then
    FValue := FMaxValue;
  if FMinValue > FMaxValue then
    FMinValue := FMaxValue;
  Invalidate;
end;

procedure TLazuliProgressBar.SetFBarShowText(AValue: boolean);
begin
  if FBarShowText=AValue then Exit;
  FBarShowText:=AValue;
  Invalidate;
end;

procedure TLazuliProgressBar.SetFBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  Invalidate;
end;

procedure TLazuliProgressBar.SetFOrientation(AValue: TProgressBarOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  Invalidate;
end;

procedure TLazuliProgressBar.SetFSmooth(AValue: boolean);
begin
  if FSmooth=AValue then Exit;
  FSmooth:=AValue;
  Invalidate;
end;

procedure TLazuliProgressBar.SetFStep(AValue: integer);
begin
  if FStep=AValue then Exit;
  FStep:=AValue;
  Invalidate;
end;

procedure TLazuliProgressBar.SetFStyle(AValue: TProgressBarStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Invalidate;
end;

end.
