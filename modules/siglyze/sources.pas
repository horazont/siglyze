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

  { TslSourceStream }

  TslSourceStream = class (TObject)
  public
    constructor Create(const ARawStream: TStream; const ASampleRate: Cardinal;
      const ARawFormatTranscoder: TFormatTranscoder;
      const ARawSampleSize: SizeUInt);
    destructor Destroy; override;
  private
    FEndOfStream: Boolean;
    FSampleRate: Cardinal;
    FSampleSize: SizeUInt;
    FRawStream: TStream;
    FTranscoder: TFormatTranscoder;
    FRawBuffer: Pointer;
  public
    function ReadSamples(const Buffer: PDouble; const Count: SizeUInt): SizeUInt;
  public
    property EndOfStream: Boolean read FEndOfStream;
    property SampleRate: Cardinal read FSampleRate;
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
      const ARawFormat: TRawFormat);
  private
    FSampleRate: Cardinal;
    FRawFormat: TRawFormat;
  public
    function CreateStream: TslSourceStream; override;
  public
    property SampleRate: Cardinal read FSampleRate;
    property RawFormat: TRawFormat read FRawFormat;
  end;

  { TslsiglyzeSource }

  TslsiglyzeSource = class (TslSource)
  public
    function CreateStream: TslSourceStream; override;
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
  const ASampleRate: Cardinal; const ARawFormatTranscoder: TFormatTranscoder;
  const ARawSampleSize: SizeUInt);
begin
  FRawStream := ARawStream;
  FTranscoder := ARawFormatTranscoder;
  FSampleRate := ASampleRate;
  FEndOfStream := False;
  FSampleSize := ARawSampleSize;
  FRawBuffer := nil;
end;

destructor TslSourceStream.Destroy;
begin
  if FRawBuffer <> nil then
    FreeMem(FRawBuffer);
  FRawStream.Free;
  inherited Destroy;
end;

function TslSourceStream.ReadSamples(const Buffer: PDouble;
  const Count: SizeUInt): SizeUInt;
var
  RawLength: SizeUInt;
  Read: SizeUInt;
begin
  if FEndOfStream then
    Exit(0);
  if FTranscoder <> nil then
  begin
    RawLength := FSampleSize * Count;
    ReAllocMem(FRawBuffer, RawLength);
    Read := FRawStream.Read(FRawBuffer^, RawLength);
    FEndOfStream := Read < RawLength;
    Result := Read div FSampleSize;
    FTranscoder(FRawBuffer, Buffer, Result);
  end
  else
  begin
    RawLength := Count * SizeOf(Double);
    Read := FRawStream.Read(Buffer^, RawLength);
    FEndOfStream := Read < RawLength;
    Result := Read div FSampleSize;
  end;
end;

{ TslSource }

constructor TslSource.Create(const AURI: TURI);
begin
  FURI := AURI;
end;

{ TslRawSource }

constructor TslRawSource.Create(const AURI: TURI; const ASampleRate: Cardinal;
  const ARawFormat: TRawFormat);
begin
  inherited Create(AURI);
  FSampleRate := ASampleRate;
  FRawFormat := ARawFormat;
  if not (FRawFormat in AllFormats) then
    raise Exception.Create('Invalid sample format for pcm input.');
end;

function TslRawSource.CreateStream: TslSourceStream;
begin
  Exit(TslSourceStream.Create(TGTURIStream.ActualStream(FURI, omRead), FSampleRate, RawFormatConverters[FRawFormat], RawFormatSize[FRawFormat]));
end;

{ TslsiglyzeSource }

function TslsiglyzeSource.CreateStream: TslSourceStream;
var
  SiglyzeStream: TStream;
  SampleRate: Cardinal;
  SampleFormatRaw: Cardinal;
  SampleFormat: TRawFormat;
begin
  SiglyzeStream := TGTURIStream.ActualStream(FURI, omRead);
  CheckRead(SiglyzeStream, SampleRate, SizeOf(Cardinal));
  CheckRead(SiglyzeStream, SampleFormatRaw, SizeOf(Cardinal));
  SampleFormat := TRawFormat(SampleFormatRaw);
  if not (SampleFormat in AllFormats) then
    raise Exception.Create('Invalid sample format in siglyze stream.');
  Exit(TslSourceStream.Create(SiglyzeStream, SampleRate, RawFormatConverters[SampleFormat], RawFormatSize[SampleFormat]));
end;

end.
