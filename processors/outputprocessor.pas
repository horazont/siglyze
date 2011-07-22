unit OutputProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProcessingOvermind, DataTypeFFT, DataTypeSamples,
  DataTypeStatus, GTNodes, GTDebug;

type
  TDataElement = (deStatus, dePCM, deFFT);
  TDataSet = set of TDataElement;

  TSiglyzeFrameHeader = packed record
    Status: TStatusRecord;
    PCMChannelCount: Cardinal;
    FFTCount: Cardinal;
  end;

  { TOutputProcessor }

  TOutputProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
       const AOwnerNode: TGTNode); override;
  private
    FDataSet: TDataSet;
    FFFTCount: Cardinal;
    FOutStream: TStream;
    FPCMChannelCount: Cardinal;

    FPCMOffset, FFFTOffset: SizeUInt;
  protected
    procedure Burn; override;
    procedure Init; override;
    function ProcessDataSet(const AInputData: TGTNodeDataSet;
       const AOutputData: TGTNodeDataSet): Boolean; override;
    procedure SetupIO; override;
  public
    property DataSet: TDataSet read FDataSet write FDataSet;
    property FFTCount: Cardinal read FFFTCount write FFFTCount;
    property OutStream: TStream read FOutStream write FOutStream;
    property PCMChannelCount: Cardinal read FPCMChannelCount write FPCMChannelCount;
  end;

implementation

{ TOutputProcessor }

constructor TOutputProcessor.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  inherited Create(AOvermind, AOwnerNode);
  FDataSet := [deStatus];
  FFFTCount := 0;
  FOutStream := nil;
  FPCMChannelCount := 0;
end;

procedure TOutputProcessor.Burn;
begin
  inherited Burn;
end;

procedure TOutputProcessor.Init;
var
  I: Integer;
begin
  Assert(FOutStream <> nil);
  for I := 0 to High(FInPorts) do
    Assert(FInPorts[I].DataType <> nil);
  inherited Init;
end;

function TOutputProcessor.ProcessDataSet(const AInputData: TGTNodeDataSet;
  const AOutputData: TGTNodeDataSet): Boolean;
var
  Header: TSiglyzeFrameHeader;
  I, J: Integer;
  SampleCount: Cardinal;
  FFTSize: Cardinal;
begin
  Header.Status := PStatusRecord(AInputData[0])^;
  Header.PCMChannelCount := FPCMChannelCount;
  Header.FFTCount := FFFTCount;
  FOutStream.Write(Header, SizeOf(TSiglyzeFrameHeader));
  FInPorts[0].DataType.FreeItem(AInputData[0]);

  J := 1;
  for I := 0 to FPCMChannelCount - 1 do
  begin
    SampleCount := TDataTypeSamples(FInPorts[J].DataType).SamplesPerBlock;
    FOutStream.Write(SampleCount, SizeOf(Cardinal));
    FOutStream.Write(AInputData[J]^, SizeOf(Double) * SampleCount);
    FInPorts[J].DataType.FreeItem(AInputData[J]);
    Inc(J);
  end;
  for I := 0 to FFFTCount - 1 do
  begin
    FFTSize := TDataTypeFFT(FInPorts[J].DataType).FFTSize;
    FOutStream.Write(FFTSize, SizeOf(Cardinal));
    FOutStream.Write(AInputData[J]^, SizeOf(Double) * FFTSize);
    FInPorts[J].DataType.FreeItem(AInputData[J]);
    Inc(J);
  end;
  if Header.Status.EndOfStream then
  begin
    DebugMsg('End of stream', [], Self);
    PostCommand(NODE_THREAD_COMMAND_PAUSE, True);
    Exit(False);
  end;
  Result := True;
end;

procedure TOutputProcessor.SetupIO;
var
  Ports: array of TGTNodeDataTypeClass;
  I, PortIdx, PortCount: Integer;

  function AssignPort(const AType: TGTNodeDataTypeClass): Integer; inline;
  begin
    Ports[PortIdx] := AType;
    Result := PortIdx;
    Inc(PortIdx);
  end;

begin
  PortCount := 1;
  if dePCM in FDataSet then
    PortCount += FPCMChannelCount;
  if deFFT in FDataSet then
    PortCount += FFFTCount;
  SetLength(Ports, PortCount);
  PortIdx := 0;
  AssignPort(TDataTypeStatus);
  if dePCM in FDataSet then
  begin
    FPCMOffset := PortIdx;
    for I := 0 to FPCMChannelCount - 1 do
      AssignPort(TDataTypeSamples);
  end;
  if deFFT in FDataSet then
  begin
    FFFTOffset := PortIdx;
    for I := 0 to FFFTCount - 1 do
      AssignPort(TDataTypeFFT);
  end;
  SetupInPorts(Ports);
  inherited SetupIO;
end;

end.

