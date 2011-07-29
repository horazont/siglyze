unit FFTProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fftw, WindowFunctions, GTNodes, ProcessingOvermind,
  DataTypeSamples, DataTypeFFT, GTMessages, ProcessingSubchannels,
  GTStreamUtils;

type

  { TFFTProcessor }

  TFFTProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
      const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FrameSize, FFFTSize: Cardinal;
    FFT: fftw_plan;
    BufferSize: SizeUInt;
    FFTInBuffer: PDouble;
    FFTOutBuffer: Pcomplex_double;

    FWindowFunction: TWindowFunctionClass;

    FFFTType: TDataTypeFFT;
    procedure SetFFTSize(const AValue: Cardinal);
  protected
    procedure DoPostFFTSize;
  protected
    procedure Burn; override;
    procedure Init; override;
    procedure Loop; override;
    procedure SetupIO; override;
  public
    property FFTSize: Cardinal read FFFTSize write SetFFTSize;
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
  FFTInBuffer := nil;
  FFTOutBuffer := nil;
  fftw_getmem(FFTInBuffer, BufferSize);
  fftw_getmem(FFTOutBuffer, BufferSize);
  FFT := fftw_plan_dft_1d(FrameSize, FFTInBuffer, FFTOutBuffer, []);
  Set8087CW($133F);
  DoPostFFTSize;
  inherited;
end;

procedure TFFTProcessor.Loop;
var
  I: Integer;
  Curr: Pcomplex_double;
  Tmp: Double;
begin
  if not TestRead(FInPorts[0], FFTInBuffer^, BufferSize) then
    ProcessSubchannelAsMessages(FInPorts[0]);

  FWindowFunction.Apply(FFTInBuffer, FrameSize);

  fftw_execute(FFT);

  Curr := FFTOutBuffer;
  for I := 0 to FFTSize - 1 do
  begin
    Tmp := Sqrt(Sqr(Curr^.im) + Sqr(Curr^.re));
    FOutPorts[0].Write(Tmp, SizeOf(Double));
    Inc(Curr);
  end;
end;

procedure TFFTProcessor.DoPostFFTSize;
begin
  FOutPorts[0].WriteSubchannel(MsgFFTSizeChanged(FFFTSize), SizeOf(TGTMessage));
end;

procedure TFFTProcessor.SetupIO;
begin
  SetupInPorts([TDataTypeSamples]);
  SetupOutPorts([FFFTType]);
  inherited SetupIO;
end;

end.

