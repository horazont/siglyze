unit InputThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sources, BlockMemoryManager, Processing;

type

  { TInputThread }

  TInputThread = class (TThread)
  public
    constructor Create(const AInputStream: TslSourceStream; ASamplesPerFrame: Cardinal;
      AMemoryManager: TBlockMemoryManager; ATargetQueue: TInputQueue; ACreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  private
    FInputStream: TslSourceStream;
    FMemoryManager: TBlockMemoryManager;
    FSamplesPerFrame: Cardinal;
    FTargetQueue: TInputQueue;
  public
    procedure Execute; override;
    property Terminated;
  end;

implementation

{ TInputThread }

constructor TInputThread.Create(const AInputStream: TslSourceStream;
  ASamplesPerFrame: Cardinal; AMemoryManager: TBlockMemoryManager;
  ATargetQueue: TInputQueue; ACreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(ACreateSuspended, StackSize);
  FSamplesPerFrame := ASamplesPerFrame;
  FInputStream := AInputStream;
  FMemoryManager := AMemoryManager;
  FTargetQueue := ATargetQueue;
end;

destructor TInputThread.Destroy;
begin
  FInputStream.Free;
  inherited Destroy;
end;

procedure TInputThread.Execute;
var
  Buffer: PDouble;
  Read, I: SizeUInt;
begin
  try
    while not Terminated do
    begin
      Buffer := PDouble(FMemoryManager.GetFreeBlock);
      Read := FInputStream.ReadSamples(Buffer, FSamplesPerFrame);
      if Read < FSamplesPerFrame then
      begin
        for I := Read to FSamplesPerFrame - 1 do
          Buffer[I] := 0.0;
        Terminate;
      end;
//      WriteLn('input thread sent block');
      FTargetQueue.Push(Buffer);
    end;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Input thread crashed with: ', E.Message);
    end;
  end;
end;

end.

