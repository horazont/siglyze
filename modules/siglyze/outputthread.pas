unit OutputThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlockMemoryManager, Processing, GTStreamUtils;

type

  { TOutputThread }

  TOutputThread = class (TThread)
  public
    constructor Create(const OutStream: TStream; SourceQueue: TOutputQueue;
      const AFFTMemoryManager, ASampleDataMemoryManager: TBlockMemoryManager;
      const ASampleRate: Cardinal;
      ACreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
  private
    FOutStream: TStream;
    FSourceQueue: TOutputQueue;
    FFFTMemoryManager: TBlockMemoryManager;
    FSampleDataMemoryManager: TBlockMemoryManager;
  public
    procedure Execute; override;
    property Terminated;
  end;

implementation

{ TOutputThread }

constructor TOutputThread.Create(const OutStream: TStream;
  SourceQueue: TOutputQueue; const AFFTMemoryManager,
  ASampleDataMemoryManager: TBlockMemoryManager; const ASampleRate: Cardinal;
  ACreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(ACreateSuspended, StackSize);
  FOutStream := OutStream;
  FSourceQueue := SourceQueue;
  FFFTMemoryManager := AFFTMemoryManager;
  FSampleDataMemoryManager := ASampleDataMemoryManager;
  OutStream.Write(ASampleRate, SizeOf(Cardinal));
end;

procedure TOutputThread.Execute;
var
  Data: PsiglyzeBlock;
begin
  try
    while not Terminated do
    begin
      while FSourceQueue.Empty do
      begin
        if Terminated then Exit;
        Sleep(1);
      end;
      Data := PsiglyzeBlock(FSourceQueue.Pop);
      FOutStream.Write(Data^.SampleCount, SizeOf(Cardinal));
      FOutStream.Write(Data^.Max, SizeOf(Double));
      FOutStream.Write(Data^.Average, SizeOf(Double));
      FOutStream.Write(Data^.PeakThreshold, SizeOf(Double));
      FOutStream.Write(Data^.LowLevelCutoff, SizeOf(Double));
      FOutStream.Write(Data^.Samples^, SizeOf(Double)*Data^.SampleCount);
      FOutStream.Write(Data^.FFT^, SizeOf(Double)*(Data^.SampleCount div 2 + 1));
      FFFTMemoryManager.ReleaseBlock(Data^.FFT);
      FSampleDataMemoryManager.ReleaseBlock(Data^.Samples);
      Dispose(Data);
    end;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Output thread crashed with: ', E.Message);
    end;
  end;
end;

end.

