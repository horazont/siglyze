unit InputProcessor; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes, DataTypeSamples, DataTypeStatus,
  ProcessingOvermind, Sources, GTDebug;

type

  { TInputProcessor }

  TInputProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
       const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FBuffer: TMultiChannelBuffer;
    FChannelCount: Cardinal;
    FSamplesPerLoop: Cardinal;
    FSourceStream: TslSourceStream;

    FSampleType: TDataTypeSamples;
    FStatusType: TDataTypeStatus;
  protected
    procedure Burn; override;
    procedure Init; override;
    procedure Loop; override;
    procedure SetupIO; override;
  public
    property SamplesPerBlock: Cardinal read FSamplesPerLoop write FSamplesPerLoop;
    property SourceStream: TslSourceStream read FSourceStream write FSourceStream;
  end;

implementation

{ TInputProcessor }

constructor TInputProcessor.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  inherited Create(AOvermind, AOwnerNode);
  FSourceStream := nil;
  FStatusType := TDataTypeStatus.Create;
  FSampleType := TDataTypeSamples.Create;
  FSamplesPerLoop := 1024;
end;

destructor TInputProcessor.Destroy;
begin
  FSampleType.Free;
  FStatusType.Free;
  inherited Destroy;
end;

procedure TInputProcessor.Burn;
var
  I: Integer;
begin
  for I := 0 to High(FBuffer) do
  begin
    FreeMem(FBuffer[I]);
    FBuffer[I] := nil;
  end;
  inherited Burn;
  FSampleType.Burn;
  FStatusType.Burn;
end;

procedure TInputProcessor.Init;
var
  I: Integer;
begin
  if FSamplesPerLoop < 1024 then
    FSamplesPerLoop := 1024;
  Assert(FSourceStream <> nil);
  SetLength(FBuffer, FSourceStream.ChannelCount);
  FStatusType.Init;
  FSampleType.Init;
  for I := 0 to High(FBuffer) do
    FBuffer[I] := GetMem(FSamplesPerLoop * SizeOf(Double));
  inherited Init;
end;

procedure TInputProcessor.Loop;
var
  I: Cardinal;
  Status: TStatusRecord;
begin
  DebugMsg('Reading %d samples', [FSamplesPerLoop], Self);
  FSourceStream.ReadSamples(FBuffer, FSamplesPerLoop);
  DebugMsg('Got sample block', [], Self);

  with Status do
  begin
    EndOfStream := FSourceStream.EndOfStream;
    if EndOfStream then
    begin
      DebugMsg('End of stream', [], Self);
      PostCommand(NODE_THREAD_COMMAND_PAUSE, True);
    end;
  end;

  FOutPorts[0].Write(Status, SizeOf(TStatusRecord));
  for I := 0 to High(FBuffer) do
    FOutPorts[I+1].Write(FBuffer[I], FSamplesPerLoop * SizeOf(Double));
end;

procedure TInputProcessor.SetupIO;
var
  Ports: array of TGTNodeDataType;
  I: Integer;
begin
  FChannelCount := FSourceStream.ChannelCount;
  SetLength(Ports, FChannelCount+1);
  Ports[0] := FStatusType;
  for I := 1 to High(Ports) do
    Ports[I] := FSampleType;
  SetupOutPorts(Ports);
  inherited SetupIO;
end;

end.

