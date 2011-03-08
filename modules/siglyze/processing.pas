unit Processing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stwThreadQueue, fftw, WindowFunctions;

type
  TInputBlock = record
    Samples: array of Double;
  end;

  TsiglyzeBlock = record
    Samples: array of Double;
    // Peaks: array of TPeak;
    FFT: array of Double;
    Max, Average, PeakThreshold, LowLevelCutoff: Double;
  end;

  TProcessingConfig = record
    SampleRate: Cardinal;
    FFTSize: Cardinal;
    WindowFunction: TWindowFunction;
  end;

  TInputQueue = specialize TstwGThreadQueue<TInputBlock>;
  TOutputQueue = specialize TstwGThreadQueue<TsiglyzeBlock>;

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
  private
    // working data, no prefixes here
    FFT: fftw_plan;
    FFTInBuffer: PDouble;
    FFTOutBuffer: Pcomplex_double;
    FFTOutSize: SizeUInt;
  protected
    procedure Burn;
    procedure BurnFFTW;
    procedure Init;
    procedure InitFFTW;
    procedure Run(const InData: TInputBlock; var OutData: TsiglyzeBlock);
  public
    procedure Execute; override;
  public
    property ApproxFPS: Double read FApproxFPS;
    property SampleRate: Cardinal read FSampleRate;
    property SamplesPerFrame: Cardinal read FSamplesPerFrame;
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
end;

destructor TProcessingThread.Destroy;
begin
  FOutputQueue.Free;
  FInputQueue.Free;
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
  FFTOutSize := FSamplesPerFrame div 2 + 1;
  FFTInBuffer := nil;
  FFTOutBuffer := nil;
  fftw_getmem(FFTInBuffer, SizeOf(Double) * FSamplesPerFrame);
  fftw_getmem(FFTOutBuffer, SizeOf(Double) * 2 * FFTOutSize);
  FFT := fftw_plan_dft_1d(FSamplesPerFrame, FFTInBuffer, FFTOutBuffer, []);
end;

procedure TProcessingThread.Run(const InData: TInputBlock;
  var OutData: TsiglyzeBlock);
begin
  Assert(Length(InData.Samples) = FSamplesPerFrame);

  OutData.Samples := InData.Samples;
  Move(OutData.Samples[0], FFTInBuffer[0], FSamplesPerFrame);

end;

procedure TProcessingThread.Execute;
var
  InputData: TInputBlock;
  OutputData: TsiglyzeBlock;
begin
  try
    Init;
    while not Terminated do
    begin
      while FInputQueue.Empty do
        Sleep(1);
      FInputQueue.Decapitate(InputData);
      OutputData.FFT := nil;
      OutputData.Samples := nil;
      Run(InputData, OutputData);
      FOutputQueue.Push(OutputData);
    end;
    while not FInputQueue.Empty do
      FInputQueue.Decapitate(InputData);
    Burn;
  except
    // todo -- exception handling here
  end;
end;

end.

