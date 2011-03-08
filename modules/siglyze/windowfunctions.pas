unit WindowFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  math;

type

  TWindowFunction = class (TObject)
  public
    procedure Apply(DataSet: PDouble; Count: SizeUInt); virtual; abstract;
  end;

  { TWindowFunctionRect }

  TWindowFunctionRect = class (TWindowFunction)
  public
    procedure Apply(DataSet: PDouble; Count: SizeUInt); override;
  end;

  { TWindowFunctionBlackman }

  TWindowFunctionBlackman = class (TWindowFunctionRect)
  public
    procedure Apply(DataSet: PDouble; Count: SizeUInt); override;
  end;

implementation

{ TWindowFunctionRect }

procedure TWindowFunctionRect.Apply(DataSet: PDouble; Count: SizeUInt);
begin

end;

{ TWindowFunctionBlackman }

procedure TWindowFunctionBlackman.Apply(DataSet: PDouble; Count: SizeUInt);
var
  I: SizeUInt;
begin
  for I := 0 to Count - 1 do
  begin
    DataSet^ := DataSet^ * 0.5 * (1+cos(2*Pi*((I / Count)+0.5)));
  end;
end;

end.

