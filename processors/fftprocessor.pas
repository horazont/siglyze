unit FFTProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fftw, WindowFunctions, GTNodes, ProcessingOvermind,
  DataTypeSamples, DataTypeFFT;

type

  { TFFTProcessor }

  TFFTProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
      const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FFT: fftw_plan;
    FFTInBuffer: PDouble;
    FFTOutBuffer: Pcomplex_double;
    FrameSize, FFTOutSize: SizeUInt;

    FWindowFunction: TWindowFunctionClass;

    FFFTType: TDataTypeFFT;
  protected
    procedure ParametrizeFFTType(Sender: TObject);
  protected
    procedure Burn; override;
    procedure Init; override;
    function ProcessDataSet(const AInputData: TGTNodeDataSet;
       const AOutputData: TGTNodeDataSet): Boolean; override;
    procedure SetupIO; override;
  public
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

procedure TFFTProcessor.ParametrizeFFTType(Sender: TObject);
begin
  if FInPorts[0].DataType = nil then
  begin
    FrameSize := 0;
    WriteLn('fft processor has no valid input');
  end
  else with FInPorts[0].DataType as TDataTypeSamples do
  begin
    Parametrize;
    Self.FrameSize := SamplesPerBlock;
  end;
  FFTOutSize := FrameSize div 2 + 1;
  FFFTType.FFTSize := FFTOutSize;
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

  FFTInBuffer := nil;
  FFTOutBuffer := nil;
  fftw_getmem(FFTInBuffer, SizeOf(Double) * FrameSize);
  fftw_getmem(FFTOutBuffer, SizeOf(Double) * 2 * FFTOutSize);
  FFT := fftw_plan_dft_1d(FrameSize, FFTInBuffer, FFTOutBuffer, []);
  Set8087CW($133F);
  inherited;
end;

function TFFTProcessor.ProcessDataSet(const AInputData: TGTNodeDataSet;
  const AOutputData: TGTNodeDataSet): Boolean;
var
  I: Integer;
  Curr: Pcomplex_double;
  Target: PDouble;
begin
  Move(AInputData[0]^, FFTInBuffer[0], FrameSize * SizeOf(Double));
  FWindowFunction.Apply(FFTInBuffer, FrameSize);
  FInPorts[0].DataType.FreeItem(AInputData[0]);

  fftw_execute(FFT);

  Curr := FFTOutBuffer;
  AOutputData[0] := FOutPorts[0].DataType.GetItem;
  Target := PDouble(AOutputData[0]);
  for I := 0 to FFTOutSize - 1 do
  begin
    Target^ := Sqrt(Sqr(Curr^.im) + Sqr(Curr^.re));
    Inc(Curr);
    Inc(Target);
  end;
  Result := True;
end;

procedure TFFTProcessor.SetupIO;
begin
  FFFTType.OnParametrize := @ParametrizeFFTType;
  SetupInPorts([TDataTypeSamples]);
  SetupOutPorts([FFFTType]);
  inherited SetupIO;
end;

end.

