unit DataTypeFFT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes;

type

  { TDataTypeFFT }

  TDataTypeFFT = class (TGTNodeDataType)
  public
    constructor Create;
  private
    FFFTSize: Cardinal;
    procedure SetFFTSize(const AValue: Cardinal);
  published
    property FFTSize: Cardinal read FFFTSize write FFFTSize;
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
  FFFTSize := AValue;
end;

end.

