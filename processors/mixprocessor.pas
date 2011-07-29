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
    FSamplesPerLoop: Cardinal;
    FBufferSize: SizeUInt;

    FInputBuffer: PDouble;
    FOutputBuffer: PDouble;

    FSampleType: TDataTypeSamples;
    procedure SetInputChannelCount(const AValue: Cardinal);
    procedure SetOutputChannelType(const AValue: TProcessingChannel);
    procedure SetSamplesPerLoop(const AValue: Cardinal);
  protected
    procedure Burn; override;
    procedure Init; override;
    procedure Loop; override;
    procedure SetupIO; override;
  public
    property InputChannelCount: Cardinal read FInputChannelCount write SetInputChannelCount;
    property OutputChannelType: TProcessingChannel read FOutputChannelType write SetOutputChannelType;
    property SamplesPerLoop: Cardinal read FSamplesPerLoop write SetSamplesPerLoop;
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
  SamplesPerLoop := 1024;
end;

destructor TMixProcessor.Destroy;
begin
  FSampleType.Free;
  inherited Destroy;
end;

procedure TMixProcessor.SetSamplesPerLoop(const AValue: Cardinal);
begin
  if FSamplesPerLoop = AValue then exit;
  FLoopLock.Enter;
  try
    FSamplesPerLoop := AValue;
    FBufferSize := FSamplesPerLoop * SizeOf(Double);
    if State >= nsInitialized then
    begin
      ReallocMem(FInputBuffer, FBufferSize);
      ReAllocMem(FOutputBuffer, FBufferSize);
    end;
  finally
    FLoopLock.Release;
  end;
end;

procedure TMixProcessor.SetInputChannelCount(const AValue: Cardinal);
begin
  ForceMaxState(nsBurned);
  if FInputChannelCount = AValue then exit;
  FInputChannelCount := AValue;
end;

procedure TMixProcessor.SetOutputChannelType(const AValue: TProcessingChannel);
begin
  ForceMaxState(nsIOSetup);
  if FOutputChannelType = AValue then exit;
  FOutputChannelType := AValue;
  FSampleType.Channel := FOutputChannelType;
end;

procedure TMixProcessor.Burn;
begin
  FreeMem(FInputBuffer);
  FreeMem(FOutputBuffer);
  inherited Burn;
end;

procedure TMixProcessor.Init;
begin
//  FOutPorts[0].DataType.Parametrize;
  if FSamplesPerLoop < 1024 then
    SamplesPerLoop := 1024;
  FSampleType.Init;
  FInputBuffer := GetMem(FBufferSize);
  FOutputBuffer := GetMem(FBufferSize);
  inherited Init;
end;

procedure TMixProcessor.Loop;
var
  I, J: Integer;
  InputPtr, OutputPtr: PDouble;
  Channels: Double;
  ReadBytes: SizeUInt;
begin
  Channels := FInputChannelCount;
  FillQWord(FOutputBuffer^, FSamplesPerLoop, QWord(Double(0.0)));
  for I := 0 to FInputChannelCount - 1 do
  begin
    ReadBytes := FInPorts[I].Read(FInputBuffer^, FBufferSize);
    if ReadBytes < FBufferSize then
    begin
      ProcessSubchannelAsMessages(FInPorts[I]);
      FillByte((FInputBuffer + ReadBytes)^, FBufferSize - ReadBytes, 0);
    end;
    OutputPtr := FOutputBuffer;
    InputPtr := FInputBuffer;
    for J := 1 to FSamplesPerLoop do
    begin
      OutputPtr^ += InputPtr^ / Channels;
      Inc(OutputPtr);
      Inc(InputPtr);
    end;
  end;
  FOutPorts[0].Write(FOutputBuffer^, FBufferSize);
end;

procedure TMixProcessor.SetupIO;
var
  Ports: array of TGTNodeDataTypeClass;
  I: Integer;
begin
  FSampleType.Channel := FOutputChannelType;
  SetupOutPorts([FSampleType]);
  SetLength(Ports, FInputChannelCount);
  for I := 0 to FInputChannelCount - 1 do
    Ports[I] := TDataTypeSamples;
  SetupInPorts(Ports);
  inherited SetupIO;
end;

end.

