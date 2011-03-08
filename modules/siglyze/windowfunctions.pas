unit WindowFunctions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TWindowFunctionRect = class (TObject)
  public
    procedure Apply(DataSet: PDouble; Count: SizeUInt); virtual; abstract;
  end;

implementation

end.

