unit DataTypeSamples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes, ProcessingOvermind;

type

  { TDataTypeSamples }

  TDataTypeSamples = class (TGTNodeDataType)
  public
    constructor Create;
  private
    FChannel: TProcessingChannel;
    FSamplesPerBlock: Cardinal;
  published
    property Channel: TProcessingChannel read FChannel write FChannel;
    property SamplesPerBlock: Cardinal read FSamplesPerBlock write FSamplesPerBlock;
  end;

implementation

{ TDataTypeSamples }

constructor TDataTypeSamples.Create;
begin
  inherited Create;
  FSamplesPerBlock := 0;
  FChannel := pcMixed;
end;

end.

