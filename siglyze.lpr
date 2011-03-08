program siglyze;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, URIParser, ioConfig, GTURIAutoRegister, GTURI,
  GTProtocolFD, GTProtocolFile, GTProtocolTCP, GTProtocolTCPSSL,
  GTProtocolUnixSock, GTUnixSockStream, Sources, Processing, fftw;

type
  EInvalidArgument = class (Exception);

  { Tsiglyze }

  Tsiglyze = class(TCustomApplication)
  protected
    procedure DoConfig;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Tsiglyze }

procedure Tsiglyze.DoConfig;
var
  ErrorMsg: String;
  SourceURI: TURI;
  SourceStr: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hs:d:pf:r:', ['help', 'source:', 'sig', 'pcm', 'sample-rate:', 'resolution:', 'dest:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not HasOption('s', 'source') then
    SourceStr := Config.DefaultSource
  else
    SourceStr := GetOptionValue('s', 'source');

  SourceURI := ParseURI(SourceStr);
  if SourceURI.Protocol = '' then
    raise EInvalidArgument.CreateFmt('%s: Not a valid source (no protocol given).', [SourceStr]);

end;

procedure Tsiglyze.DoRun;
begin
  DoConfig;
  { add your program here }

  // stop program loop
  Terminate;
end;

constructor Tsiglyze.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tsiglyze.Destroy;
begin
  inherited Destroy;
end;

procedure Tsiglyze.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: Tsiglyze;

{$R *.res}

begin
  Application:=Tsiglyze.Create(nil);
  Application.Run;
  Application.Free;
end.

