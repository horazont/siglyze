unit POTSyncProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes, ProcessingOvermind, ProcessingSubchannels,
  DataTypeSamples, DataTypeFFT, DataTypeStatus, GTMessages, GTDebug;

type

  TSyncBuffer = record
    Data: Pointer;
    Offset, FullAt: SizeUInt;
    IsFull: Boolean;
  end;

  { TPOTSyncProcessor }

  TPOTSyncProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
       const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FSyncFrameSize: Cardinal;
    FSyncBufferSize: SizeUInt;
    FSyncBuffers: array of TSyncBuffer;
    FOutTypes: array of TGTNodeDataType;
    FInFFTSizes: array of Cardinal;
    FInputFFTCount: TGTNodePortNumber;
    FInputSamplesCount: TGTNodePortNumber;
    FMinSyncFrameSize: Cardinal;
    procedure SetInputFFTCount(const AValue: TGTNodePortNumber);
    procedure SetInputSamplesCount(const AValue: TGTNodePortNumber);
    procedure SetMinSyncFrameSize(const AValue: Cardinal);
    procedure SetSyncFrameSize(const AValue: Cardinal);
  protected
    procedure HandleSubchannel(const AInPortIndex: TGTNodePortNumber);
    procedure ReallocSyncBuffers;
    function ReadToSyncBuffer(const ASource: TStream; var ADest: TSyncBuffer): Boolean;
    procedure RecalculateSyncFrameSize;
  protected
    procedure Burn; override;
    procedure Init; override;
    procedure Loop; override;
    procedure SetupIO; override;
  protected
    property SyncFrameSize: Cardinal read FSyncFrameSize write SetSyncFrameSize;
  public
    property InputFFTCount: TGTNodePortNumber read FInputFFTCount write SetInputFFTCount;
    property InputSamplesCount: TGTNodePortNumber read FInputSamplesCount write SetInputSamplesCount;
    property MinSyncFrameSize: Cardinal read FMinSyncFrameSize write SetMinSyncFrameSize;
  end;

function IsPOT(const Number: Cardinal): Boolean;

implementation

function IsPOT(const Number: Cardinal): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to 31 do
  begin
    if Number and (1 shl I) <> 0 then
    begin
      Result := not Result;
      if not Result then
        Exit;
    end;
  end;
end;

{ TPOTSyncProcessor }

constructor TPOTSyncProcessor.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  inherited Create(AOvermind, AOwnerNode);
  FMinSyncFrameSize := 1024;
end;

destructor TPOTSyncProcessor.Destroy;
begin
  inherited Destroy;
end;

procedure TPOTSyncProcessor.SetInputFFTCount(const AValue: TGTNodePortNumber);
begin
  ForceMaxState(nsBurned);
  if FInputFFTCount = AValue then exit;
  FInputFFTCount := AValue;
end;

procedure TPOTSyncProcessor.SetInputSamplesCount(const AValue: TGTNodePortNumber
  );
begin
  ForceMaxState(nsBurned);
  if FInputSamplesCount = AValue then exit;
  FInputSamplesCount := AValue;
end;

procedure TPOTSyncProcessor.SetMinSyncFrameSize(const AValue: Cardinal);
begin
  ForceMaxState(nsInitialized);
  if FMinSyncFrameSize = AValue then exit;
  if not IsPOT(AValue) then
    Exit;
  FLoopLock.Acquire;
  try
    FMinSyncFrameSize := AValue;
  finally
    FLoopLock.Release;
  end;
end;

procedure TPOTSyncProcessor.SetSyncFrameSize(const AValue: Cardinal);
begin
  if FSyncFrameSize = AValue then exit;
  FLoopLock.Acquire;
  try
    FSyncFrameSize := AValue;
    FSyncBufferSize := FSyncFrameSize * SizeOf(Double);
    if State = nsInitialized then
      ReallocSyncBuffers;
  finally
    FLoopLock.Release;
  end;
end;

procedure TPOTSyncProcessor.HandleSubchannel(
  const AInPortIndex: TGTNodePortNumber);
var
  Msg: PGTMessage;
  MsgSize: SizeUInt;
begin
  if not FInPorts[AInPortIndex].ReadSubchannel(Msg, MsgSize) then
    Exit;
  case Msg^.Msg of
    PSC_FFT_SIZE_CHANGED:
    begin
      if not (FOutTypes[AInPortIndex] is TDataTypeFFT) then
      begin
        DebugMsg('FFT_SIZE_CHANGED received on non-fft port %d.', [AInPortIndex], Self);
        Exit;
      end;
      FOutPorts[AInPortIndex].WriteSubchannel(Msg, MsgSize);
      FInFFTSizes[AInPortIndex] := Msg^.Value;
      // sync frame must not be smaller than this, as reallocation may kill our
      // data otherwise.
      FMinSyncFrameSize := SyncFrameSize;
      RecalculateSyncFrameSize;
    end;
  else
    DebugMsg('Unknown subchannel message received at port %d.', [AInPortIndex], Self);
  end;
end;

procedure TPOTSyncProcessor.ReallocSyncBuffers;
var
  I: Integer;
begin
  for I := 0 to High(FSyncBuffers) do
  begin
    ReAllocMem(FSyncBuffers[I].Data, FSyncBufferSize);
    FSyncBuffers[I].Offset := 0;
    if I < FInputFFTCount then
      FSyncBuffers[I].FullAt := FInFFTSizes[I] * SizeOf(Double)
    else
      FSyncBuffers[I].FullAt := FSyncBufferSize;
  end;
end;

function TPOTSyncProcessor.ReadToSyncBuffer(const ASource: TStream;
  var ADest: TSyncBuffer): Boolean;
var
  ReadCount, ActuallyRead: SizeUInt;
begin
  ReadCount := ADest.FullAt - ADest.Offset;
  ActuallyRead := ASource.Read((ADest.Data + ADest.Offset)^, ReadCount);
  ADest.Offset += ActuallyRead;
  Result := ActuallyRead = ReadCount;
  ADest.IsFull := Result;
  if Result then
    ADest.Offset := 0;
end;

procedure TPOTSyncProcessor.RecalculateSyncFrameSize;
var
  I: Integer;
  CurrFFTSize, MaxFFTSize: Cardinal;
begin
  MaxFFTSize := 0;
  for I := 0 to High(FInFFTSizes) do
  begin
    CurrFFTSize := FInFFTSizes[I];
    if not IsPOT(CurrFFTSize) then
      raise EGTNodeError.CreateFmt('Invalid FFT size for power-of-two sync processor: %d.', [CurrFFTSize]);

    CurrFFTSize *= 2;
    if CurrFFTSize > MaxFFTSize then
      MaxFFTSize := CurrFFTSize;
  end;
  FLoopLock.Acquire;
  if MaxFFTSize < FMinSyncFrameSize then
    MaxFFTSize := FMinSyncFrameSize;
  FLoopLock.Release;
  SyncFrameSize := MaxFFTSize;
end;

procedure TPOTSyncProcessor.Burn;
var
  I: Integer;
begin
  for I := 0 to High(FSyncBuffers) do
    FreeMem(FSyncBuffers[I].Data);
  inherited Burn;
  for I := 0 to High(FOutTypes) do
    FOutTypes[I].Free;
end;

procedure TPOTSyncProcessor.Init;
var
  I: Integer;
begin
  RecalculateSyncFrameSize;
  SetLength(FSyncBuffers, FInputFFTCount + FInputSamplesCount);
  for I := 0 to High(FSyncBuffers) do
  begin
    FSyncBuffers[I].Data := GetMem(FSyncBufferSize);
    FSyncBuffers[I].Offset := 0;
    FSyncBuffers[I].FullAt := FSyncBufferSize;
  end;
  for I := 0 to FInputFFTCount - 1 do
    FSyncBuffers[I].FullAt := FInFFTSizes[I] * SizeOf(Double);
  for I := FInputFFTCount to FInputFFTCount + (FInputSamplesCount - 1) do
    TDataTypeSamples(FOutTypes[I]).Channel := TDataTypeSamples(FInPorts[I].DataType).Channel;
  SetLength(FInFFTSizes, FInputFFTCount);
  FillDWord(FInFFTSizes[0], FInputFFTCount, 0);
  inherited Init;
end;

procedure TPOTSyncProcessor.Loop;
var
  I: Integer;
  AllFull: Boolean;
begin
  AllFull := True;
  for I := 0 to High(FInPorts) do
  begin
    if FSyncBuffers[I].IsFull then
      Continue;
    AllFull := ReadToSyncBuffer(FInPorts[I], FSyncBuffers[I]) and AllFull;
  end;

  if not AllFull then
  begin
    for I := 0 to High(FSyncBuffers) do
      if not FSyncBuffers[I].IsFull then
      begin
        HandleSubchannel(I);
      end;
  end
  else
  begin
    for I := 0 to High(FSyncBuffers) do
    begin
      FOutPorts[I].Write(FSyncBuffers[I].Data, FSyncBuffers[I].FullAt);
      FSyncBuffers[I].IsFull := False;
      FSyncBuffers[I].Offset := 0;
    end;
  end;
end;

procedure TPOTSyncProcessor.SetupIO;
var
  I: Integer;
  InTypes: array of TGTNodeDataTypeClass;
begin
  for I := 0 to High(FOutTypes) do
    FOutTypes[I].Free;
  SetLength(FOutTypes, FInputFFTCount + FInputSamplesCount);
  SetLength(InTypes, Length(FOutTypes));
  for I := 0 to FInputFFTCount - 1 do
  begin
    FOutTypes[I] := TDataTypeFFT.Create;
    InTypes[I] := TDataTypeFFT;
  end;
  for I := FInputFFTCount to FInputFFTCount + (FInputSamplesCount - 1) do
  begin
    FOutTypes[I] := TDataTypeSamples.Create;
    InTypes[I] := TDataTypeSamples;
  end;
  inherited SetupIO;
end;

end.

