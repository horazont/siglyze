program siglyze;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, URIParser, ioConfig, GTURIAutoRegister, GTURI,
  GTProtocolFD, GTProtocolFile, GTProtocolTCP, GTProtocolTCPSSL,
  GTProtocolUnixSock, GTUnixSockStream, Sources, Processing, fftw,
  WindowFunctions, InputThread, BlockMemoryManager, OutputThread, typinfo,
  math, GTStuff;

type
  EInvalidArgument = class (Exception);

  { Tsiglyze }

  Tsiglyze = class(TCustomApplication)
  private
    FInputThread: TInputThread;
    FProcessingThread: TProcessingThread;
    FOutputThread: TOutputThread;
  protected
    procedure DoConfig;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Tsiglyze }

procedure Tsiglyze.DoConfig;
var
  ErrorMsg: String;
  SourceURI: TURI;
  SourceStr: String;
  Source: TslSource;
  SourceStream: TslSourceStream;
  ProcessingConfig: TProcessingConfig;
  FPS: Double;
  WindowFunction: TWindowFunction;
  S: String;
  Dest: TStream;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hs:d:pf:r:w:', ['help', 'source:', 'sig', 'pcm', 'sample-rate:', 'resolution:', 'dest:', 'fft-size:', 'fps:', 'window:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not HasOption('w', 'window') then
    WindowFunction := TWindowFunctionBlackman.Create
  else
  begin
    S := LowerCase(GetOptionValue('w', 'window'));
    if S = 'blackman' then
      WindowFunction := TWindowFunctionBlackman.Create
    else if S = 'rect' then
      WindowFunction := TWindowFunctionRect.Create
    else
      raise EInvalidArgument.CreateFmt('Unknown window function: %s.', [S]);
  end;

  if not HasOption('s', 'source') then
    SourceStr := Config.DefaultSource
  else
    SourceStr := GetOptionValue('s', 'source');

  SourceURI := ParseURI(SourceStr);
  if SourceURI.Protocol = '' then
    raise EInvalidArgument.CreateFmt('%s: Not a valid source (no protocol given).', [SourceStr]);

  {$ifopt R+}
  {$define WasR}
  {$R-}
  {$endif}
  {$ifopt Q+}
  {$define WasQ}
  {$Q-}
  {$endif}
  FPS := NaN;
  {$ifdef WasQ}
  {$undef WasQ}
  {$Q-}
  {$endif}
  {$ifdef WasR}
  {$undef WasR}
  {$R+}
  {$endif}
  if HasOption('fft-size') then
    ProcessingConfig.FFTSize := StrToInt(GetOptionValue('fft-size'))
  else if HasOption('fps') then
    FPS := StrToInt(GetOptionValue('fps'))
  else
    raise EInvalidArgument.Create('Must have either fft-size or fps.');

  if HasOption('p', 'pcm') then
    Source := TslRawSource.Create(SourceURI, StrToInt(GetOptionValue('f', 'sample-rate')), TRawFormat(GetEnumValue(TypeInfo(TRawFormat), 'rf'+GetOptionValue('r', 'resolution'))))
  else
    Source := TslsiglyzeSource.Create(SourceURI);
  SourceStream := Source.CreateStream;
  Source.Free;

  if not HasOption('d', 'dest') then
    Dest := TGTURIStream.ActualStream(ParseURI(Config.DefaultDest), omWrite, wmIgnore, smDontCare)
  else
    Dest := TGTURIStream.ActualStream(ParseURI(GetOptionValue('d', 'dest')), omWrite, wmIgnore, smDontCare);

  if not IsNaN(FPS) then
    ProcessingConfig.FFTSize := Round(SourceStream.SampleRate / FPS);
  ProcessingConfig.SampleRate := SourceStream.SampleRate;
  ProcessingConfig.WindowFunction := WindowFunction;

  FProcessingThread := TProcessingThread.Create(ProcessingConfig, True);
  FInputThread := TInputThread.Create(SourceStream, ProcessingConfig.FFTSize, FProcessingThread.SampleDataMemoryManager, FProcessingThread.InputQueue, True);
  FOutputThread := TOutputThread.Create(Dest, FProcessingThread.OutputQueue, FProcessingThread.FFTDataMemoryManager, FProcessingThread.SampleDataMemoryManager, ProcessingConfig.SampleRate, True);
end;

procedure Tsiglyze.DoRun;
begin
  DoConfig;

  WriteLn(ErrOutput, Format('Going to run on approx %.2f Hz, sample rate is %d Hz, fft width is %d.', [FProcessingThread.ApproxFPS, FProcessingThread.SampleRate, FProcessingThread.SamplesPerFrame]));
  WriteLn(ErrOutput, Format('Each data frame will have a size of: %s', [FormatDataSize(FProcessingThread.SamplesPerFrame * SizeOf(Double) + 4 * SizeOf(Double) + 1 * SizeOf(Cardinal) + (FProcessingThread.SamplesPerFrame div 2 + 1) * SizeOf(Double))]));
  WriteLn(ErrOutput, Format('Expected output data rate: %s/s.', [FormatDataSize(FProcessingThread.SampleRate * SizeOf(Double) + 4 * SizeOf(Double) + 1 * SizeOf(Cardinal) + FProcessingThread.ApproxFPS * (FProcessingThread.SamplesPerFrame div 2 + 1) * SizeOf(Double))]));

  FInputThread.Resume;
  FProcessingThread.Resume;
  FOutputThread.Resume;

  try
    while not (FProcessingThread.Terminated) do
    begin
      if FInputThread.Terminated then
        FProcessingThread.Terminate;
    end;
  except

  end;
  FInputThread.Terminate;
  FInputThread.WaitFor;
  FProcessingThread.Terminate;
  FProcessingThread.WaitFor;
  FOutputThread.Terminate;
  FOutputThread.WaitFor;

  FOutputThread.Free;
  FInputThread.Free;
  FProcessingThread.Free;

  Terminate;
end;

constructor Tsiglyze.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tsiglyze.Destroy;
begin
  inherited Destroy;
end;

procedure Tsiglyze.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: Tsiglyze;

{$R *.res}

begin
  Application:=Tsiglyze.Create(nil);
  Application.Run;
  Application.Free;
end.

