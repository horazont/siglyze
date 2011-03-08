unit InputThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sources;

type

  { TInputThread }

  TInputThread = class (TThread)
  public
    constructor Create(const AInputStream: TslSourceStream; ASamplesPerFrame: Cardinal;
      ACreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  private
    FInputStream: TslSourceStream;
    FSamplesPerFrame: Cardinal;
  public
    procedure Execute; override;
  end;

implementation

{ TInputThread }

constructor TInputThread.Create(const AInputStream: TslSourceStream;
  ASamplesPerFrame: Cardinal; ACreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited Create(ACreateSuspended, StackSize);
  FSamplesPerFrame := ASamplesPerFrame;
  FInputStream := AInputStream;
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
    Buffer := GetMem(SizeOf(Double) * FSamplesPerFrame);
    try
      while not Terminated do
      begin
        Read := FInputStream.ReadSamples(Buffer, FSamplesPerFrame);
        if Read < FSamplesPerFrame then
        begin
          for I := Read to FSamplesPerFrame - 1 do
            Buffer[I] := 0.0;
        end;
      end;
    finally
      FreeMem(Buffer);
    end;
  except

  end;
end;

end.

