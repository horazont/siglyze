{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit siglyze; 

interface

uses
  DataTypeFFT, DataTypeSamples, DataTypeStatus, ProcessingOvermind, 
  FFTProcessor, InputProcessor, MixProcessor, OutputProcessor, SyncProcessor, 
  Sources, WindowFunctions, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('siglyze', @Register); 
end.
