unit DataTypeStatus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodeDataTypeBlockMemory, GTNodes;

type
  TSourceType = (stFile, stStream);
  TDataType = (dtAudio, dtRadio);

  TFileInfo = packed record
    Length: QWord; // in millisecs
    Position: QWord;
  end;

  TRadioInfo = packed record
    CenterFrequency: Double;
    LowestFrequency: Double;
  end;

  TStatusRecord = packed record
    EndOfStream: Boolean;
    SourceType: TSourceType;
    DataType: TDataType;
    FileInfo: TFileInfo;
    RadioInfo: TRadioInfo;
  end;
  PStatusRecord = ^TStatusRecord;

  { TDataTypeStatus }

  TDataTypeStatus = class (TGTNodeDataTypeCustomBlockMemory)
  public
    constructor Create;
  end;

implementation

{ TDataTypeStatus }

constructor TDataTypeStatus.Create;
begin
  inherited Create;
  Size := SizeOf(TStatusRecord);
end;

end.

