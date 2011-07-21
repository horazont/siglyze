unit MixProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProcessingOvermind, DataTypeSamples, GTNodes;

type
  EMixerError = class (Exception);

  { TMixProcessor }

  TMixProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
       const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FActualChannelCount: Cardinal;
    FInputChannelCount: Cardinal;
    FOutputChannelType: TProcessingChannel;
    FSampleCount: Cardinal;

    FSampleType: TDataTypeSamples;
  protected
    procedure Init; override;
    procedure ParametrizeOutput(Sender: TObject);
    function ProcessDataSet(const AInputData: TGTNodeDataSet;
       const AOutputData: TGTNodeDataSet): Boolean; override;
    procedure SetupIO; override;
  public
    property InputChannelCount: Cardinal read FInputChannelCount write FInputChannelCount;
    property OutputChannelType: TProcessingChannel read FOutputChannelType write FOutputChannelType;
  end;

implementation

{ TMixProcessor }

constructor TMixProcessor.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  inherited Create(AOvermind, AOwnerNode);
  FInputChannelCount := 2;
  FOutputChannelType := pcMixed;
  FSampleType := TDataTypeSamples.Create;
end;

destructor TMixProcessor.Destroy;
begin
  FSampleType.Free;
  inherited Destroy;
end;

procedure TMixProcessor.Init;
begin
//  FOutPorts[0].DataType.Parametrize;
  FSampleType.Init;
  inherited Init;
end;

procedure TMixProcessor.ParametrizeOutput(Sender: TObject);
var
  I: Integer;
  SampleCount: Cardinal;
begin
  FActualChannelCount := 0;
  SampleCount := 0;
  for I := 0 to FInputChannelCount - 1 do
    if FInPorts[I].DataType <> nil then
    begin
      if SampleCount = 0 then
        SampleCount := TDataTypeSamples(FInPorts[I].DataType).SamplesPerBlock
      else if SampleCount <> TDataTypeSamples(FInPorts[I].DataType).SamplesPerBlock then
        raise EMixerError.Create('Sample count per block is not equal for all sources.');
      Inc(FActualChannelCount);
    end;
  FSampleType.SamplesPerBlock := SampleCount;
  FSampleCount := SampleCount;
end;

function TMixProcessor.ProcessDataSet(const AInputData: TGTNodeDataSet;
  const AOutputData: TGTNodeDataSet): Boolean;
var
  I, J: Integer;
  InputPtr, OutputPtr: PDouble;
begin
  AOutputData[0] := FOutPorts[0].DataType.GetItem;
  for I := 0 to FInputChannelCount - 1 do
  begin
    OutputPtr := PDouble(AOutputData[0]);
    if FInPorts[I].DataType <> nil then
    begin
      InputPtr := PDouble(AInputData[I]);
      for J := 1 to FSampleCount do
      begin
        OutputPtr^ += InputPtr^ / FActualChannelCount;
        Inc(OutputPtr);
        Inc(InputPtr);
      end;
      FInPorts[I].DataType.FreeItem(AInputData[I]);
    end;
  end;
  Result := True;
end;

procedure TMixProcessor.SetupIO;
var
  Ports: array of TGTNodeDataTypeClass;
  I: Integer;
begin
  FSampleType.Channel := FOutputChannelType;
  FSampleType.OnParametrize := @ParametrizeOutput;
  SetupOutPorts([FSampleType]);
  SetLength(Ports, FInputChannelCount);
  for I := 0 to FInputChannelCount - 1 do
    Ports[I] := TDataTypeSamples;
  SetupInPorts(Ports);
  inherited SetupIO;
end;

end.

