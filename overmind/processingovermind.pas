unit ProcessingOvermind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GTNodes;

type
  TProcessingChannel = (pcUndefined, pcMixed, pcFrontLeft, pcFrontRight, pcCenter, pcRearLeft, pcRearRight, pcSubwoofer);

type
  TProcessingOvermind = class (TGTNodeOvermind)
  end;

  TProcessingThread_ = specialize TGTTypedNodeThread<TProcessingOvermind>;
  TProcessor = class (TGTNodeThread);

implementation

end.

