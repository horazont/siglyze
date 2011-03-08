unit Processing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stwThreadStack, fftw, WindowFunctions, BlockMemoryManager;

type
  TsiglyzeBlock = record
    SampleCount: Cardinal;
    Samples: PDouble;
    // Peaks: array of TPeak;
    FFT: PDouble;
    Max, Average, PeakThreshold, LowLevelCutoff: Double;
  end;
  PsiglyzeBlock = ^TsiglyzeBlock;

  TProcessingConfig = record
    SampleRate: Cardinal;
    FFTSize: Cardinal;
    WindowFunction: TWindowFunction;
  end;

  TInputQueue = TstwThreadStack;
  TOutputQueue = TstwThreadStack;

  { TProcessingThread }

  TProcessingThread = class (TThread)
  public
    constructor Create(const ProcessingConfig: TProcessingConfig;
      ACreateSuspended: Boolean; const AStackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  private
    FApproxFPS: Double;
    FInputQueue: TInputQueue;
    FOutputQueue: TOutputQueue;
    FSampleRate: Cardinal;
    FSamplesPerFrame: Cardinal;
    FWindowFunction: TWindowFunction;
  private
    FFFTDataMemoryManager: TBlockMemoryManager;
    // working data, no prefixes here
    FFT: fftw_plan;
    FFTInBuffer: PDouble;
    FFTOutBuffer: Pcomplex_double;
    FFTOutSize: SizeUInt;
    FSampleDataMemoryManager: TBlockMemoryManager;
  protected
    procedure Burn;
    procedure BurnFFTW;
    procedure Init;
    procedure InitFFTW;
    procedure Run(const InData: PDouble; var OutData: TsiglyzeBlock);
  public
    procedure Execute; override;
  public
    property ApproxFPS: Double read FApproxFPS;
    property SampleRate: Cardinal read FSampleRate;
    property SamplesPerFrame: Cardinal read FSamplesPerFrame;
    property SampleDataMemoryManager: TBlockMemoryManager read FSampleDataMemoryManager;
    property FFTDataMemoryManager: TBlockMemoryManager read FFFTDataMemoryManager;
    property OutputQueue: TOutputQueue read FOutputQueue;
    property InputQueue: TInputQueue read FInputQueue;
    property Terminated;
  end;

implementation

{ TProcessingThread }

constructor TProcessingThread.Create(const ProcessingConfig: TProcessingConfig;
  ACreateSuspended: Boolean; const AStackSize: SizeUInt);
begin
  inherited Create(ACreateSuspended, AStackSize);
  FSampleRate := ProcessingConfig.SampleRate;
  FSamplesPerFrame := ProcessingConfig.FFTSize;
  FApproxFPS := FSampleRate / FSamplesPerFrame;
  FInputQueue := TInputQueue.Create;
  FOutputQueue := TOutputQueue.Create;
  FWindowFunction := ProcessingConfig.WindowFunction;
  FFTOutSize := FSamplesPerFrame div 2 + 1;
  FSampleDataMemoryManager := TBlockMemoryManager.Create(SizeOf(Double)*FSamplesPerFrame, 32);
  FFFTDataMemoryManager := TBlockMemoryManager.Create(SizeOf(Double)*FFTOutSize, 32);
end;

destructor TProcessingThread.Destroy;
begin
  FFFTDataMemoryManager.Free;
  FSampleDataMemoryManager.Free;
  FOutputQueue.Free;
  FInputQueue.Free;
  FWindowFunction.Free;
  inherited Destroy;
end;

procedure TProcessingThread.Burn;
begin
  BurnFFTW;
end;

procedure TProcessingThread.BurnFFTW;
begin
  fftw_destroy_plan(FFT);
  fftw_freemem(FFTOutBuffer);
  fftw_freemem(FFTInBuffer);
end;

procedure TProcessingThread.Init;
begin
  InitFFTW;
end;

procedure TProcessingThread.InitFFTW;
begin
  FFTInBuffer := nil;
  FFTOutBuffer := nil;
  fftw_getmem(FFTInBuffer, SizeOf(Double) * FSamplesPerFrame);
  fftw_getmem(FFTOutBuffer, SizeOf(Double) * 2 * FFTOutSize);
  FFT := fftw_plan_dft_1d(FSamplesPerFrame, FFTInBuffer, FFTOutBuffer, []);
end;

procedure TProcessingThread.Run(const InData: PDouble;
  var OutData: TsiglyzeBlock);
var
  I: Integer;
  Curr: Pcomplex_double;
begin
  OutData.SampleCount := FSamplesPerFrame;
  OutData.Samples := InData;
  Move(OutData.Samples[0], FFTInBuffer[0], FSamplesPerFrame);
  FWindowFunction.Apply(FFTInBuffer, FSamplesPerFrame);

  fftw_execute(FFT);
  OutData.FFT := PDouble(FFFTDataMemoryManager.GetFreeBlock);
  Curr := FFTOutBuffer;
  for I := 0 to FFTOutSize - 1 do
  begin
    OutData.FFT[I] := Sqrt(Sqr(Curr^.im) + Sqr(Curr^.re));
    Inc(Curr);
  end;
end;

procedure TProcessingThread.Execute;
var
  InputData: PDouble;
  OutputData: PsiglyzeBlock;
begin
  try
    Init;
    try
      while (not Terminated) or (not FInputQueue.Empty) do
      begin
        while FInputQueue.Empty do
        begin
          if Terminated then
            Exit;
          Sleep(1);
        end;
//        WriteLn('processing thread received block');
        New(OutputData);
        InputData := PDouble(FInputQueue.Pop);
        Run(InputData, OutputData^);
        FOutputQueue.Push(OutputData);
//        WriteLn('processing thread sent block');
      end;
    finally
      Burn;
    end;
  except
    on E: Exception do
    begin
      WriteLn(stderr, 'Processing thread crashed with ', E.Message);
    end;
  end;
end;

end.

