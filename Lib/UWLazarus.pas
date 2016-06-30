{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit UWLazarus;

interface

uses
  UWControls, UWControls.WaveformDisplay, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UWControls', @UWControls.Register);
  RegisterUnit('UWControls.WaveformDisplay', 
    @UWControls.WaveformDisplay.Register);
end;

initialization
  RegisterPackage('UWLazarus', @Register);
end.
