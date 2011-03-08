unit ioConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTConfig, GTXDG;

type

  { TslXDG }

  TslXDG = class (TGTXDGPaths)
  public
    class function GetAppName: String; override;
    class function GetXDGConfigDirName: String; override;
    class function GetXDGDataDirName: String; override;
  end;

  { TslConfig }

  TslConfig = class (TGTConfig)
  public
    constructor Create; override;
  private
    FDefaultDest: String;
    FDefaultSource: String;
  published
    property DefaultSource: String read FDefaultSource write FDefaultSource;
    property DefaultDest: String read FDefaultDest write FDefaultDest;
  end;

var
  Config: TslConfig = nil;

implementation

{ TslXDG }

class function TslXDG.GetAppName: String;
begin
  Result := 'siglyze';
end;

class function TslXDG.GetXDGConfigDirName: String;
begin
  Result := 'sigsuite/'+GetAppName;
end;

class function TslXDG.GetXDGDataDirName: String;
begin
  Result := 'sigsuite/'+GetAppName;
end;

{ TslConfig }

constructor TslConfig.Create;
begin
  inherited Create;
  FDefaultSource := 'fd://stdin/';
  FDefaultDest := 'fd://stdout/';
end;

initialization
XDG := TslXDG;
Config := TslConfig.Create;
Config.Load;

finalization
Config.Save;
FreeAndNil(Config);

end.

