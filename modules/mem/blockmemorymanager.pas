unit BlockMemoryManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stwThreadStack{, stwCriticalSections};

type
  TBlockStack = class (TstwCustomThreadStack);

  TBlockGroup = PPointer;

  { TBlockMemoryManager }

  TBlockMemoryManager = class (TObject)
  public
    constructor Create(const BlockSize, BlocksPerGroup: SizeUInt);
    destructor Destroy; override;
  private
    FBlockSize, FBlocksPerGroup: SizeUInt;
    FFreeBlocks: TBlockStack;
    FBlockLock: TRTLCriticalSection;
    FBlocks: array of TBlockGroup;
    FBlockCapacity, FBlockCount: Integer;
  protected
    procedure Expand;
  public
    function GetFreeBlock: Pointer;
    procedure ReleaseBlock(const ABlock: Pointer);
  end;

implementation

{ TBlockMemoryManager }

constructor TBlockMemoryManager.Create(const BlockSize, BlocksPerGroup: SizeUInt
  );
begin
  FBlockSize := BlockSize;
  FBlocksPerGroup := BlocksPerGroup;
  FFreeBlocks := TBlockStack.Create;
//  FBlockLock := NewCriticalSection;
  FBlockCapacity := 0;
  FBlockCount := 0;
  InitCriticalSection(FBlockLock);
end;

destructor TBlockMemoryManager.Destroy;
var
  I, J: Integer;
begin
  DoneCriticalsection(FBlockLock);
//  DisposeCriticalSection(FBlockLock);
  for I := 0 to High(FBlocks) do
  begin
    if FBlocks[I] <> nil then
    begin
      for J := 0 to FBlocksPerGroup-1 do
        FreeMem(FBlocks[I][J]);
      FreeMem(FBlocks[I]);
    end;
  end;
  FFreeBlocks.Free;
  inherited Destroy;
end;

procedure TBlockMemoryManager.Expand;
var
  I, J: Integer;
  CurrBlock: PPointer;
begin
  EnterCriticalSection(FBlockLock);
  try
    if FBlockCount = FBlockCapacity then
    begin
      FBlockCapacity += 8;
      SetLength(FBlocks, FBlockCapacity);
      for I := FBlockCount to FBlockCapacity - 1 do
        FBlocks[I] := nil;
    end;
    I := FBlockCount;
    FBlocks[I] := GetMem(FBlocksPerGroup*SizeOf(Pointer));
    CurrBlock := FBlocks[I];
    for J := 1 to FBlocksPerGroup do
    begin
      CurrBlock^ := GetMem(FBlockSize);
      FFreeBlocks.Push(CurrBlock^);
      Inc(CurrBlock);
    end;
    Inc(FBlockCount);
  finally
    LeaveCriticalSection(FBlockLock);
  end;
end;

function TBlockMemoryManager.GetFreeBlock: Pointer;
begin
  while FFreeBlocks.Empty do
    Expand;
  Exit(FFreeBlocks.Pop);
end;

procedure TBlockMemoryManager.ReleaseBlock(const ABlock: Pointer);
begin
  FFreeBlocks.Push(ABlock);
end;

end.

