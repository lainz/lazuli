{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazuli;

interface

uses
  LazuliButton, LazuliTheme, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazuliButton', @LazuliButton.Register);
end;

initialization
  RegisterPackage('lazuli', @Register);
end.
