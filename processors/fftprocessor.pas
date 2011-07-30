unit FFTProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fftw, WindowFunctions, GTNodes, ProcessingOvermind,
  DataTypeSamples, DataTypeFFT, GTMessages, ProcessingSubchannels,
  GTStreamUtils, GTDebug, GTRingBuffer;

type

  { TFFTProcessor }

  TFFTProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
      const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FOverlap: Cardinal;
    FrameSize, FFFTSize: Cardinal;
    FFT: fftw_plan;
    BufferSize: SizeUInt;
    FFTInBuffer: PDouble;
    FFTOutBuffer: Pcomplex_double;
    NewSamplesPerFrame: Cardinal;
    NewBytesPerFrame: SizeUInt;

    Backbuffer: PDouble;

    FWindowFunction: TWindowFunctionClass;

    FFFTType: TDataTypeFFT;
    procedure SetFFTSize(const AValue: Cardinal);
    procedure SetOverlap(const AValue: Cardinal);
  protected
    procedure DoPostFFTSize;
    procedure DoPostFFTSampleEquivalent;
  protected
    procedure Burn; override;
    procedure Init; override;
    procedure Loop; override;
    procedure SetupIO; override;
  public
    property FFTSize: Cardinal read FFFTSize write SetFFTSize;
    property Overlap: Cardinal read FOverlap write SetOverlap;
    property WindowFunction: TWindowFunctionClass read FWindowFunction write FWindowFunction;
  end;

implementation

{ TFFTProcessor }

constructor TFFTProcessor.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  inherited Create(AOvermind, AOwnerNode);
  FWindowFunction := TWindowFunctionBlackman;
  FFFTType := TDataTypeFFT.Create;
end;

destructor TFFTProcessor.Destroy;
begin
  FFFTType.Free;
  inherited Destroy;
end;

procedure TFFTProcessor.SetFFTSize(const AValue: Cardinal);
begin
  if FFFTSize = AValue then exit;
  if AValue < 1 then
    Exit;
  FLoopLock.Acquire;
  try
    FFFTSize := AValue;
    if State = nsInitialized then
      DoPostFFTSize;
  finally
    FLoopLock.Release;
  end;
end;

procedure TFFTProcessor.SetOverlap(const AValue: Cardinal);
begin
  if FOverlap = AValue then exit;
  FLoopLock.Acquire;
  try
    FOverlap := AValue;
    if FOverlap >= FFFTSize*2 then
      FOverlap := FFFTSize*2 - 1;
    NewSamplesPerFrame := FFFTSize*2 - FOverlap;
    NewBytesPerFrame := NewSamplesPerFrame * SizeOf(Double);
    if State >= nsInitialized then
      DoPostFFTSampleEquivalent;
  finally
    FLoopLock.Release;
  end;
end;

procedure TFFTProcessor.Burn;
begin
  fftw_destroy_plan(FFT);
  fftw_freemem(FFTOutBuffer);
  fftw_freemem(FFTInBuffer);
  FFFTType.Burn;
  inherited;
end;

procedure TFFTProcessor.Init;
begin
  FFFTType.Init;

  FrameSize := FFFTSize * 2;
  BufferSize := SizeOf(Double) * FrameSize;


  {WriteLn(Format('%d %d', [NewBytesPerFrame, BufferSize]));
  Halt(0);}
  Backbuffer := GetMem(BufferSize);
  FillByte(Backbuffer^, BufferSize, $00);
  FFTInBuffer := nil;
  FFTOutBuffer := nil;
  fftw_getmem(FFTInBuffer, BufferSize);
  fftw_getmem(FFTOutBuffer, BufferSize);
  FFT := fftw_plan_dft_1d(FrameSize, FFTInBuffer, FFTOutBuffer, []);
  Set8087CW($133F);
  DoPostFFTSize;
  DoPostFFTSampleEquivalent;
  inherited;
end;

procedure TFFTProcessor.Loop;
var
  I: Integer;
  Curr: Pcomplex_double;
  Target: PDouble;
  Tmp: Double;
begin
  if FOverlap = 0 then
  begin
    CheckRead(FInPorts[0], FFTInBuffer^, BufferSize);
  end
  else
  begin
    {DebugMsg('Moving buffer backwards', [], Self);
    DebugMsg('Reading new data', [], Self);
    DebugMsg('Swapping buffers ', [], Self);
    Move(Backbuffer^, FFTInBuffer^, BufferSize);}
    {DebugMsg('Moving buffer of size %d backwards by %d bytes.', [BufferSize, NewBytesPerFrame]);
    FillByte(Backbuffer^, BufferSize, 0);
    Move((Backbuffer + NewBytesPerFrame)^, Backbuffer^, (BufferSize - NewBytesPerFrame));
    DebugMsg('Writing at offset %d.', [(BufferSize - NewBytesPerFrame)]);
    CheckRead(FInPorts[0], (Backbuffer + (BufferSize - NewBytesPerFrame))^, NewBytesPerFrame);}
    Move((Backbuffer + NewSamplesPerFrame)^, Backbuffer^, (BufferSize - NewBytesPerFrame));
    CheckRead(FInPorts[0], (Backbuffer + (FrameSize - NewSamplesPerFrame))^, NewBytesPerFrame);
    Move(Backbuffer^, FFTInBuffer^, BufferSize);
  end;

  FWindowFunction.Apply(FFTInBuffer, FrameSize);

  fftw_execute(FFT);

  Curr := FFTOutBuffer;
  Target := PDouble(FFTOutBuffer);
  for I := 0 to FFTSize - 1 do
  begin
    Target^ := Sqrt(Sqr(Curr^.im) + Sqr(Curr^.re));
    //FOutPorts[0].Write(Tmp, SizeOf(Double));
    Inc(Curr);
    Inc(Target);
  end;
  FOutPorts[0].Write(FFTOutBuffer^, FFFTSize * SizeOf(Double));
end;

procedure TFFTProcessor.DoPostFFTSize;
begin
  DebugMsg('Posting fft size', [], Self);
  FOutPorts[0].WriteSubchannel(MsgFFTSize(FFFTSize), SizeOf(TGTMessage));
  DebugMsg('ok', [], Self);
end;

procedure TFFTProcessor.DoPostFFTSampleEquivalent;
begin
  DebugMsg('Posting fft sample equivalent', [], Self);
  FOutPorts[0].WriteSubchannel(MsgFFTSampleEquivalent(NewSamplesPerFrame), SizeOf(TGTMessage));
  DebugMsg('ok', [], Self);
end;

procedure TFFTProcessor.SetupIO;
begin
  SetupInPorts([TDataTypeSamples]);
  SetupOutPorts([FFFTType]);
  inherited SetupIO;
end;

end.

