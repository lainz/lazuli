unit LazuliButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, LMessages, Forms, LCLType,
  BGRABitmap, BGRABitmapTypes;

type
  TLazuliButtonState = (lbsNormal, lbsHovered, lbsActive);

  { TLazuliButton }

  TLazuliButton = class(TCustomControl)
  private
    FBGRA: TBGRABitmap;
    FState: TLazuliButtonState;
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure TextChanged; override;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure UpdateFocus(AFocused: boolean);
  protected
    procedure PrepareBuffer;
    procedure DrawBuffer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    //property BidiMode;
    property BorderSpacing;
    //property Cancel;
    property Caption;
    property Color;
    property Constraints;
    //property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    //property ParentBidiMode;
    //property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

procedure Register;

implementation

uses
  LazuliTheme;

procedure Register;
begin
  RegisterComponents('Lazuli', [TLazuliButton]);
end;

{ TLazuliButton }

procedure TLazuliButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  LazuliStyle.MeasureButton(Self, FBGRA, PreferredWidth, PreferredHeight);
end;

class function TLazuliButton.GetControlClassDefaultSize: TSize;
begin
  Result:=inherited GetControlClassDefaultSize;
end;

procedure TLazuliButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if CanFocus() then
    SetFocus();
  FState := lbsActive;
  Invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TLazuliButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FState := lbsHovered;
  Invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TLazuliButton.MouseEnter;
begin
  FState := lbsHovered;
  Invalidate;
  inherited MouseEnter;
end;

procedure TLazuliButton.MouseLeave;
begin
  FState := lbsNormal;
  Invalidate;
  inherited MouseLeave;
end;

procedure TLazuliButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    FState := lbsActive;
    Invalidate;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TLazuliButton.KeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    FState := lbsNormal;
    Invalidate;
    Self.Click;
  end;

  inherited KeyUp(Key, Shift);
end;

procedure TLazuliButton.Paint;
begin
  PrepareBuffer;
  LazuliStyle.DrawButton(Self, FState, FBGRA);
  DrawBuffer;
end;

procedure TLazuliButton.TextChanged;
begin
  InvalidatePreferredSize;
  if Assigned(Parent) and Parent.AutoSize then
    Parent.AdjustSize;
  AdjustSize;
  Invalidate;
  inherited TextChanged;
end;

procedure TLazuliButton.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;

  UpdateFocus(True);
end;

procedure TLazuliButton.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;

  if Message.FocusedWnd <> Handle then
    UpdateFocus(False);
end;

procedure TLazuliButton.UpdateFocus(AFocused: boolean);
var
  lForm: TCustomForm;
begin
  lForm := GetParentForm(Self);
  if lForm = nil then
    exit;

  if AFocused then
    ActiveDefaultControlChanged(lForm.ActiveControl)
  else
    ActiveDefaultControlChanged(nil);

  Invalidate;
end;

procedure TLazuliButton.PrepareBuffer;
begin
  if (FBGRA.Width <> Width) or (FBGRA.Height <> Height) then
    FBGRA.SetSize(Width, Height);

  FBGRA.FillTransparent;
end;

procedure TLazuliButton.DrawBuffer;
begin
  FBGRA.Draw(Canvas, 0, 0, False);
end;

constructor TLazuliButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBGRA := TBGRABitmap.Create(Width, Height);
  FState := lbsNormal;
  TabStop := True;
end;

destructor TLazuliButton.Destroy;
begin
  FreeAndNil(FBGRA);
  inherited Destroy;
end;

end.
