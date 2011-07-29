unit DataTypeStatus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes;

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

  TDataTypeStatus = class (TGTNodeDataType)
  end;

implementation

end.

