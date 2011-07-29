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
  published
    property Channel: TProcessingChannel read FChannel write FChannel;
  end;

implementation

{ TDataTypeSamples }

constructor TDataTypeSamples.Create;
begin
  inherited Create;
  FChannel := pcMixed;
end;

end.

