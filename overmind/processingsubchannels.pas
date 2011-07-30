unit ProcessingSubchannels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTMessages;

const
  PSC_BASE_OFFSET = GT_DERIVATE_MESSAGE;

  PSC_FFT_SIZE =                                        PSC_BASE_OFFSET + $0000;
  PSC_FFT_SAMPLE_EQUIVALENT =                           PSC_BASE_OFFSET + $0001;

function MsgFFTSize(const ANewSize: Cardinal): PGTMessage;
function MsgFFTSampleEquivalent(const ANewSampleEquivalent: Cardinal): PGTMessage;

implementation

function MsgFFTSize(const ANewSize: Cardinal): PGTMessage;
begin
  New(Result);
  Result^.Msg := PSC_FFT_SIZE;
  Result^.OwnsValue := False;
  Result^.DataType := dtInt;
  Result^.Value := ANewSize;
end;

function MsgFFTSampleEquivalent(const ANewSampleEquivalent: Cardinal
  ): PGTMessage;
begin
  New(Result);
  Result^.Msg := PSC_FFT_SAMPLE_EQUIVALENT;
  Result^.OwnsValue := False;
  Result^.DataType := dtInt;
  Result^.Value := ANewSampleEquivalent;
end;

end.

