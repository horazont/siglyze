unit SyncProcessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes, DataTypeStatus, ProcessingOvermind;

type

  { TSyncProcessor }

  TSyncProcessor = class (TProcessor)
  public
    constructor Create(const AOvermind: TGTNodeOvermind;
       const AOwnerNode: TGTNode); override;
    destructor Destroy; override;
  private
    FInPortCount: TGTNodePortNumber;
    FStatusType: TDataTypeStatus;
  protected
    procedure Burn; override;
    procedure Init; override;
    function ProcessDataSet(const AInputData: TGTNodeDataSet;
       const AOutputData: TGTNodeDataSet): Boolean; override;
    procedure SetupIO; override;
  published
    property InPortCount: TGTNodePortNumber read FInPortCount write FInPortCount;
  end;

implementation

{ TSyncProcessor }

constructor TSyncProcessor.Create(const AOvermind: TGTNodeOvermind;
  const AOwnerNode: TGTNode);
begin
  inherited Create(AOvermind, AOwnerNode);
  FStatusType := TDataTypeStatus.Create;
end;

destructor TSyncProcessor.Destroy;
begin
  FStatusType.Free;
  inherited Destroy;
end;

procedure TSyncProcessor.Burn;
begin
  inherited Burn;
  FStatusType.Burn;
end;

procedure TSyncProcessor.Init;
begin
  FStatusType.Init;
  inherited Init;
end;

function TSyncProcessor.ProcessDataSet(const AInputData: TGTNodeDataSet;
  const AOutputData: TGTNodeDataSet): Boolean;
var
  I: Integer;
begin
  AOutputData[0] := FStatusType.GetItem;
  Move(AInputData[0]^, AOutputData[0]^, FStatusType.GetSize);
  for I := 0 to FInPortCount - 1 do
    FInPorts[I].DataType.FreeItem(AInputData[I]);
end;

procedure TSyncProcessor.SetupIO;
var
  Ports: array of TGTNodeDataTypeClass;
  I: Integer;
begin
  SetLength(Ports, FInPortCount);
  {$ifdef CPU64}
  FillQWord(Ports[0], FInPortCount, QWord(TDataTypeStatus));
  {$else}
  FillDWord(Ports[0], FInPortCount, DWord(TDataTypeStatus));
  {$endif}
  SetupInPorts(Ports);
  SetupOutPorts([FStatusType]);
  inherited SetupIO;
end;

end.

