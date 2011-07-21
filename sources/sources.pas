unit Sources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTURI, URIParser, GTStreamUtils;

type
  TRawFormat = (rfSigned16Bit = 0, rfSigned32Bit = 1, rfFloat32Bit = 2, rfFloat64Bit = 3);

const
  AllFormats = [rfSigned16Bit, rfSigned32Bit, rfFloat32Bit, rfFloat64Bit];

type
  TFormatTranscoder = procedure (const InBuffer: Pointer; const OutBuffer: PDouble; const Count: SizeUInt);
  TMultiChannelBuffer = array of PDouble;
  TRawMultiChannelBuffer = array of Pointer;

  { TslSourceStream }

  TslSourceStream = class (TObject)
  public
    constructor Create(const ARawStream: TStream; const ASampleRate: Cardinal;
      const AChannelCount: Cardinal;
      const ARawFormatTranscoder: TFormatTranscoder;
      const ARawSampleSize: SizeUInt; const ASeekable, ALengthKnown: Boolean;
      const ADuration: QWord = 0);
    destructor Destroy; override;
  private
    FChannelCount: Cardinal;
    FEndOfStream: Boolean;
    FDuration: QWord;
    FLengthKnown: Boolean;
    FPosition: QWord;
    FSampleRate: Cardinal;
    FSampleSize: SizeUInt;
    FRawStream: TStream;
    FSeekable: Boolean;
    FTranscoder: TFormatTranscoder;
    FRawBuffer: TRawMultiChannelBuffer;
    FRawBufferPointers: TRawMultiChannelBuffer;
    FTotalSamplesRead: QWord;
  public
    function ReadSamples(const Buffer: TMultiChannelBuffer; const Count: SizeUInt): SizeUInt;
    procedure SetPosition(const APosition: QWord);
  public
    property ChannelCount: Cardinal read FChannelCount;
    property EndOfStream: Boolean read FEndOfStream;
    property Duration: QWord read FDuration;
    property Position: QWord read FPosition;
    property SampleRate: Cardinal read FSampleRate;
    property Seekable: Boolean read FSeekable;
    property LengthKnown: Boolean read FLengthKnown;
  end;

  TslSource = class (TObject)
  public
    constructor Create(const AURI: TURI);
  private
    FURI: TURI;
  public
    function CreateStream: TslSourceStream; virtual; abstract;
  public
    property URI: TURI read FURI;
  end;

  { TslRawSource }

  TslRawSource = class (TslSource)
  public
    constructor Create(const AURI: TURI; const ASampleRate: Cardinal;
      const AChannelCount: Cardinal; const ARawFormat: TRawFormat);
  private
    FChannelCount: Cardinal;
    FSampleRate: Cardinal;
    FRawFormat: TRawFormat;
  public
    function CreateStream: TslSourceStream; override;
  public
    property ChannelCount: Cardinal read FChannelCount;
    property SampleRate: Cardinal read FSampleRate;
    property RawFormat: TRawFormat read FRawFormat;
  end;

procedure TranscodeS16(const InBuffer: Pointer; const OutBuffer: PDouble; const Count: SizeUInt);
//procedure TranscodeS24(const InBuffer: Pointer; const OutBuffer: PDouble; const Count: SizeUInt);
procedure TranscodeS32(const InBuffer: Pointer; const OutBuffer: PDouble; const Count: SizeUInt);
procedure TranscodeF32(const InBuffer: Pointer; const OutBuffer: PDouble; const Count: SizeUInt);

const
  RawFormatConverters : array [TRawFormat] of TFormatTranscoder = (
    @TranscodeS16,
//    @TranscodeS24,
    @TranscodeS32,
    @TranscodeF32,
    nil
  );
  RawFormatSize : array [TRawFormat] of SizeUInt = (
    2,
//    3,
    4,
    4,
    8
  );

implementation

procedure TranscodeS16(const InBuffer: Pointer; const OutBuffer: PDouble;
  const Count: SizeUInt);
var
  CurrIn: PSmallInt;
  CurrOut: PDouble;
  I: SizeUInt;
begin
  CurrIn := PSmallInt(InBuffer);
  CurrOut := OutBuffer;
  for I := 1 to Count do
  begin
    CurrOut^ := CurrIn^ / High(SmallInt);
    Inc(CurrOut);
    Inc(CurrIn);
  end;
end;

{procedure TranscodeS24(const InBuffer: Pointer; const OutBuffer: PDouble;
  const Count: SizeUInt);
type
  TThreeBytes = packed record A: array [0..2] of Byte; end;
  PThreeBytes = ^TThreeBytes;
const
  Max = 16777215;
var
  CurrIn: PThreeBytes;
  CurrOut: PDouble;
  I: SizeUInt;
begin
  CurrIn := PSmallInt(InBuffer);
  CurrOut := OutBuffer;
  for I := 1 to Count do
  begin
    CurrOut^ := (CurrIn^.A[0] or (CurrIn^.A[1] shl 8) or (CurrIn^.A[2] shl 16)) / Max;
    Inc(CurrOut);
    Inc(CurrIn);
  end;
end;}

procedure TranscodeS32(const InBuffer: Pointer; const OutBuffer: PDouble;
  const Count: SizeUInt);
var
  CurrIn: PCardinal;
  CurrOut: PDouble;
  I: SizeUInt;
begin
  CurrIn := PCardinal(InBuffer);
  CurrOut := OutBuffer;
  for I := 1 to Count do
  begin
    CurrOut^ := CurrIn^ / High(Cardinal);
    Inc(CurrOut);
    Inc(CurrIn);
  end;
end;

procedure TranscodeF32(const InBuffer: Pointer; const OutBuffer: PDouble;
  const Count: SizeUInt);
var
  CurrIn: PSingle;
  CurrOut: PDouble;
  I: SizeUInt;
begin
  CurrIn := PSingle(InBuffer);
  CurrOut := OutBuffer;
  for I := 1 to Count do
  begin
    CurrOut^ := CurrIn^;
    Inc(CurrOut);
    Inc(CurrIn);
  end;
end;

{ TslSourceStream }

constructor TslSourceStream.Create(const ARawStream: TStream;
  const ASampleRate: Cardinal; const AChannelCount: Cardinal;
  const ARawFormatTranscoder: TFormatTranscoder;
  const ARawSampleSize: SizeUInt; const ASeekable, ALengthKnown: Boolean;
  const ADuration: QWord);
var
  I: Integer;
begin
  FRawStream := ARawStream;
  FTranscoder := ARawFormatTranscoder;
  FSampleRate := ASampleRate;
  FEndOfStream := False;
  FSampleSize := ARawSampleSize;
  FChannelCount := AChannelCount;
  FSeekable := ASeekable;
  FLengthKnown := ALengthKnown;
  if FLengthKnown then
    FDuration := ADuration
  else
    FDuration := 0;
  FPosition := 0;
  SetLength(FRawBuffer, AChannelCount);
  SetLength(FRawBufferPointers, AChannelCount);
  for I := 0 to High(FRawBuffer) do
    FRawBuffer[I] := nil;
end;

destructor TslSourceStream.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FRawBuffer) do
    if FRawBuffer[I] <> nil then
      FreeMem(FRawBuffer[I]);
  FRawStream.Free;
  inherited Destroy;
end;

function TslSourceStream.ReadSamples(const Buffer: TMultiChannelBuffer;
  const Count: SizeUInt): SizeUInt;
var
  RawLength: SizeUInt;
  Read, ReadSampleCount: SizeUInt;
  I, J: SizeUInt;
  ReadStep: SizeUInt;
begin
  if FEndOfStream then
    Exit(0);
  if Length(Buffer) <> FChannelCount then
    raise EGTStreamError.Create('Not enough read buffers given.');
  if FChannelCount = 1 then
  begin
    if FTranscoder <> nil then
    begin
      RawLength := FSampleSize * Count;
      ReAllocMem(FRawBuffer[0], RawLength);
      Read := FRawStream.Read(FRawBuffer[0]^, RawLength);
      FEndOfStream := Read < RawLength;
      Result := Read div FSampleSize;
      FTranscoder(FRawBuffer[0], Buffer[0], Result);
    end
    else
    begin
      RawLength := Count * SizeOf(Double);
      Read := FRawStream.Read(Buffer[0]^, RawLength);
      FEndOfStream := Read < RawLength;
      Result := Read div FSampleSize;
    end;
  end
  else
  begin
    if FTranscoder <> nil then
    begin
      ReadStep := FSampleSize;
      RawLength := ReadStep * Count;
      for I := 0 to High(FRawBuffer) do
      begin
        ReallocMem(FRawBuffer[I], RawLength);
        FRawBufferPointers[I] := FRawBuffer[I];
      end;
    end
    else
    begin
      ReadStep := SizeOf(Double);
      for I := 0 to High(FRawBuffer) do
      begin
        FRawBufferPointers[I] := Buffer[I];
      end;
    end;
    ReadSampleCount := 0;
    for I := 0 to Count - 1 do
    begin
      if FEndOfStream then
        Break;
      for J := 0 to High(FRawBufferPointers) do
      begin
        Read := FRawStream.Read(FRawBufferPointers[J]^, ReadStep);
        FEndOfStream := Read < ReadStep;
        if FEndOfStream then
          Break;
        Inc(FRawBufferPointers[J], ReadStep);
      end;
      Inc(ReadSampleCount);
    end;
    if FTranscoder <> nil then
    begin
      for I := 0 to High(FRawBuffer) do
      begin
        FTranscoder(FRawBuffer[I], Buffer[I], ReadSampleCount);
      end;
    end;
    Result := ReadSampleCount;
  end;
  FTotalSamplesRead += Result;
  FPosition := Round(FTotalSamplesRead / FSampleRate * 1000.0);
end;

procedure TslSourceStream.SetPosition(const APosition: QWord);
var
  NewSample: QWord;
begin
  if not FSeekable then
    raise EStreamError.Create('Stream is not seekable.');
  NewSample := Round((APosition / 1000.0) * FSampleRate);
  FRawStream.Position := FRawStream.Position + (NewSample - FTotalSamplesRead) * FSampleSize;
  FTotalSamplesRead := NewSample;
end;

{ TslSource }

constructor TslSource.Create(const AURI: TURI);
begin
  FURI := AURI;
end;

{ TslRawSource }

constructor TslRawSource.Create(const AURI: TURI; const ASampleRate: Cardinal;
  const AChannelCount: Cardinal; const ARawFormat: TRawFormat);
begin
  inherited Create(AURI);
  FChannelCount := AChannelCount;
  FSampleRate := ASampleRate;
  FRawFormat := ARawFormat;
  if not (FRawFormat in AllFormats) then
    raise Exception.Create('Invalid sample format for pcm input.');
end;

function TslRawSource.CreateStream: TslSourceStream;
var
  Stream: TStream;
begin
  Stream := TGTURIStream.ActualStream(FURI, omRead);
  Exit(TslSourceStream.Create(Stream, FSampleRate, FChannelCount, RawFormatConverters[FRawFormat], RawFormatSize[FRawFormat], Stream.Size > 0, Stream.Size > 0, Stream.Size div RawFormatSize[FRawFormat]));
end;

end.
