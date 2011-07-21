unit DataTypeFFT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes, GTNodeDataTypeBlockMemory;

type

  { TDataTypeFFT }

  TDataTypeFFT = class (TGTNodeDataTypeCustomBlockMemory)
  public
    constructor Create;
  private
    FFFTSize: Cardinal;
    procedure SetFFTSize(const AValue: Cardinal);
  published
    property FFTSize: Cardinal read FFFTSize write SetFFTSize;
  end;

implementation

{ TDataTypeFFT }

constructor TDataTypeFFT.Create;
begin
  inherited Create;
  FFFTSize := 0;
end;

procedure TDataTypeFFT.SetFFTSize(const AValue: Cardinal);
begin
  if FFFTSize = AValue then exit;
  Size := AValue * SizeOf(Double);
  WriteLn('fft size set to ', AValue, ' => size = ', Size);
  FFFTSize := AValue;
end;

end.

