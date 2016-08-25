unit LazuliButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, LMessages, Forms, LCLType,
  BGRABitmap, BGRABitmapTypes;

type
  TLazuliButtonState = (lbsNormal, lbsHovered, lbsActive);
  TLazuliButtonStates = set of TLazuliButtonState;

  { TLazuliButton }

  TLazuliButton = class(TCustomControl)
  private
    FActive: boolean;
    FBGRA: TBGRABitmap;
    FCancel: boolean;
    FDefault: boolean;
    FModalResult: TModalResult;
    FShortCut: TShortcut;
    FShortCutKey2: TShortcut;
    FState: TLazuliButtonStates;
    FRolesUpdateLocked: boolean;
    procedure SetCancel(AValue: boolean);
    procedure SetDefault(AValue: boolean);
    procedure SetModalResult(AValue: TModalResult);
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
    procedure WSSetDefault;
    procedure Loaded; override;
    procedure UpdateDefaultCancel;
  protected
    procedure PrepareBuffer;
    procedure DrawBuffer;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure ExecuteDefaultAction; override;
    procedure ExecuteCancelAction; override;
    procedure ActiveDefaultControlChanged(NewControl: TControl); override;
    procedure UpdateRolesForForm; override;
    function UseRightToLeftAlignment: boolean; override;
  public
    property Active: boolean read FActive stored False;
    property ShortCut: TShortcut read FShortCut;
    property ShortCutKey2: TShortcut read FShortCutKey2;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel: boolean read FCancel write SetCancel default False;
    property Caption;
    property Color;
    property Constraints;
    property Default: boolean read FDefault write SetDefault default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ModalResult: TModalResult
      read FModalResult write SetModalResult default mrNone;
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
    property TabStop default True;
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

procedure TLazuliButton.SetCancel(AValue: boolean);
var
  Form: TCustomForm;
begin
  if FCancel = AValue then
    Exit;
  FCancel := AValue;
  Form := GetParentForm(Self);
  if Assigned(Form) then
  begin
    if AValue then
      Form.CancelControl := Self
    else
      Form.CancelControl := nil;
  end;
end;

procedure TLazuliButton.SetDefault(AValue: boolean);
var
  Form: TCustomForm;
begin
  if FDefault = AValue then
    Exit;
  FDefault := AValue;
  Form := GetParentForm(Self);
  if Assigned(Form) then
  begin
    if AValue then
    begin
      Form.DefaultControl := Self;
    end
    else
    begin
      if Form.DefaultControl = Self then
        Form.DefaultControl := nil;
    end;
  end;
  WSSetDefault;
end;

procedure TLazuliButton.SetModalResult(AValue: TModalResult);
begin
  if FModalResult = AValue then
    Exit;
  FModalResult := AValue;
end;

procedure TLazuliButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  LazuliStyle.MeasureButton(Self, FBGRA, PreferredWidth, PreferredHeight);
end;

class function TLazuliButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 25;
end;

procedure TLazuliButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if CanFocus() then
    SetFocus();
  FState := FState + [lbsActive];
  Invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TLazuliButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FState := FState - [lbsActive];
  Invalidate;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TLazuliButton.MouseEnter;
begin
  FState := FState + [lbsHovered];
  Invalidate;
  inherited MouseEnter;
end;

procedure TLazuliButton.MouseLeave;
begin
  FState := FState - [lbsHovered];
  Invalidate;
  inherited MouseLeave;
end;

procedure TLazuliButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    FState := FState + [lbsActive];
    Invalidate;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TLazuliButton.KeyUp(var Key: word; Shift: TShiftState);
begin
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
  begin
    FState := FState - [lbsActive];
    Invalidate;
    if (Key = VK_SPACE) then
      Self.Click;

    if (Key = VK_RETURN) and not FDefault and not FCancel then
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

procedure TLazuliButton.WSSetDefault;
begin

end;

procedure TLazuliButton.Loaded;
begin
  inherited Loaded;
  UpdateDefaultCancel;
end;

procedure TLazuliButton.UpdateDefaultCancel;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Assigned(Form) then
  begin
    FRolesUpdateLocked := True;
    try
      if FDefault then
        Form.DefaultControl := Self;
      if FCancel then
        Form.CancelControl := Self;
    finally
      FRolesUpdateLocked := False;
    end;
  end;
  WSSetDefault;
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
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FBGRA := TBGRABitmap.Create(Width, Height);
  FState := [lbsNormal];
  FRolesUpdateLocked := False;
  ControlStyle := ControlStyle + [csHasDefaultAction, csHasCancelAction];
  TabStop := True;
end;

destructor TLazuliButton.Destroy;
begin
  FreeAndNil(FBGRA);
  inherited Destroy;
end;

procedure TLazuliButton.Click;
var
  Form: TCustomForm;
begin
  if ModalResult <> mrNone then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;
  end;
  inherited Click;
end;

procedure TLazuliButton.ExecuteDefaultAction;
begin
  if FActive or FDefault then
    Click;
end;

procedure TLazuliButton.ExecuteCancelAction;
begin
  if FCancel then
    Click;
end;

procedure TLazuliButton.ActiveDefaultControlChanged(NewControl: TControl);
var
  lPrevActive: boolean;
  lForm: TCustomForm;
begin
  lPrevActive := FActive;
  lForm := GetParentForm(Self);
  if NewControl = Self then
  begin
    FActive := True;
    if lForm <> nil then
      lForm.ActiveDefaultControl := Self;
  end
  else
  if NewControl <> nil then
    FActive := False
  else
  begin
    FActive := FDefault;
    if lForm.ActiveDefaultControl = Self then
      lForm.ActiveDefaultControl := nil;
  end;
  if lPrevActive <> FActive then
    WSSetDefault;
end;

procedure TLazuliButton.UpdateRolesForForm;
var
  AForm: TCustomForm;
  NewRoles: TControlRolesForForm;
begin
  if FRolesUpdateLocked then
    Exit;
  AForm := GetParentForm(Self);
  if not Assigned(AForm) then
    Exit; // not on a form => keep settings

  // on a form => use settings of parent form
  NewRoles := AForm.GetRolesForControl(Self);
  Default := crffDefault in NewRoles;
  Cancel := crffCancel in NewRoles;
end;

function TLazuliButton.UseRightToLeftAlignment: boolean;
begin
  Result := False;
end;

end.
