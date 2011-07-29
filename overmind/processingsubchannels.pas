unit ProcessingSubchannels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTMessages;

const
  PSC_BASE_OFFSET = GT_DERIVATE_MESSAGE;

  PSC_FFT_SIZE_CHANGED =                                PSC_BASE_OFFSET + $0000;

function MsgFFTSizeChanged(const ANewSize: Cardinal): PGTMessage;

implementation

function MsgFFTSizeChanged(const ANewSize: Cardinal): PGTMessage;
begin
  New(Result);
  Result^.Msg := PSC_FFT_SIZE_CHANGED;
  Result^.OwnsValue := False;
  Result^.DataType := dtInt;
  Result^.Value := ANewSize;
end;

end.

