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
    FSamplesPerBlock: Cardinal;
    FSourceStream: TslSourceStream;

    FSampleType: TDataTypeSamples;
    FStatusType: TDataTypeStatus;
  protected
    procedure Burn; override;
    procedure Init; override;
    function ProcessDataSet(const AInputData: TGTNodeDataSet;
       const AOutputData: TGTNodeDataSet): Boolean; override;
    procedure SetupIO; override;
  public
    property SamplesPerBlock: Cardinal read FSamplesPerBlock write FSamplesPerBlock;
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
end;

destructor TInputProcessor.Destroy;
begin
  FSampleType.Free;
  FStatusType.Free;
  inherited Destroy;
end;

procedure TInputProcessor.Burn;
begin
  inherited Burn;
  FSampleType.Burn;
  FStatusType.Burn;
end;

procedure TInputProcessor.Init;
begin
  Assert(FSourceStream <> nil);
  SetLength(FBuffer, FSourceStream.ChannelCount);
  FStatusType.Init;
  FSampleType.Init;
  inherited Init;
end;

function TInputProcessor.ProcessDataSet(const AInputData: TGTNodeDataSet;
  const AOutputData: TGTNodeDataSet): Boolean;
var
  I: Cardinal;
begin
  AOutputData[0] := FStatusType.GetItem;
  for I := 1 to FChannelCount do
  begin
    AOutputData[I] := FSampleType.GetItem;
    FBuffer[I-1] := PDouble(AOutputData[I]);
  end;
  FSourceStream.ReadSamples(FBuffer, FSamplesPerBlock);
  //PDouble(FBuffer[0])[10] := Random;
  with PStatusRecord(AOutputData[0])^ do
  begin
    EndOfStream := FSourceStream.EndOfStream;
    if EndOfStream then
    begin
      DebugMsg('End of stream', [], Self);
      Pause;
    end;
  end;
  Result := True;
end;

procedure TInputProcessor.SetupIO;
var
  Ports: array of TGTNodeDataType;
  I: Integer;
begin
  FChannelCount := FSourceStream.ChannelCount;
  SetLength(Ports, FChannelCount+1);
  FSampleType.SamplesPerBlock := FSamplesPerBlock;
  Ports[0] := FStatusType;
  for I := 1 to High(Ports) do
    Ports[I] := FSampleType;
  SetupOutPorts(Ports);
  inherited SetupIO;
end;

end.

