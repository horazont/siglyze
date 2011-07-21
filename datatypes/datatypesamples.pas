unit DataTypeSamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes, GTNodeDataTypeBlockMemory, ProcessingOvermind;

type

  { TDataTypeSamples }

  TDataTypeSamples = class (TGTNodeDataTypeCustomBlockMemory)
  public
    constructor Create;
  private
    FChannel: TProcessingChannel;
    FSamplesPerBlock: Cardinal;
    procedure SetSamplesPerBlock(const AValue: Cardinal);
  published
    property Channel: TProcessingChannel read FChannel write FChannel;
    property SamplesPerBlock: Cardinal read FSamplesPerBlock write SetSamplesPerBlock;
  end;

implementation

{ TDataTypeSamples }

constructor TDataTypeSamples.Create;
begin
  inherited Create;
  FSamplesPerBlock := 0;
  FChannel := pcMixed;
end;

procedure TDataTypeSamples.SetSamplesPerBlock(const AValue: Cardinal);
begin
  if FSamplesPerBlock = AValue then exit;
  Size := SizeOf(Double) * AValue;
  FSamplesPerBlock := AValue;
end;

end.

