unit exiftool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, syncobjs, contnrs, Forms;

type
  TExifTool = class;

  { TStringObject }

  TStringObject = class
    S: AnsiString;
  end;

  { TExifQueuedJob }

  TExifQueuedJob = class
    Tool: TExifTool;
    Callback: TDataEvent;
    FileName: UTF8String;
    constructor Create(ATool: TExifTool; CB: TDataEvent);
    procedure Execute; virtual; abstract;
  end;

  { TExifQueue }

  TExifQueue = class(TThread)
  strict private
    Tool: TExifTool;
    Event: TEvent;
    myQueue: TQueue;
  public
    constructor Create(ATool: TExifTool);
    destructor Destroy; override;
    procedure Execute; override;
    procedure WakeUp;
    procedure Terminate; reintroduce;
    procedure Enqueue(Job: TExifQueuedJob);
  end;

  { TExifTool }

  TExifTool = class(TProcess)
  strict private
    FAsyncQueue: TExifQueue;
    FError: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RunCommand(Command: TStringList): AnsiString;
    procedure StayOpenFalse;
    procedure Enqueue(Job: TExifQueuedJob);
    procedure GetThumbnail(FileName: UTF8String; CB: TDataEvent);
    procedure GetTags(FileName: UTF8String; CB: TDataEvent);
    property Error: Boolean read FError;
  end;


implementation
type
  { TExifKeywordsParseJob }

  TExifKeywordsParseJob = class(TExifQueuedJob)
    constructor Create(ATool: TExifTool; CB: TDataEvent; AFileNama: UTF8String);
    procedure Execute; override;
  end;

  { TExifGetThumbnailJob }

  TExifGetThumbnailJob = class(TExifQueuedJob)
    constructor Create(ATool: TExifTool; CB: TDataEvent; AFileNama: UTF8String);
    procedure Execute; override;
  end;

{ TExifQueue }

constructor TExifQueue.Create(ATool: TExifTool);
begin
  Event := TEvent.Create(nil, False, False, '');
  myQueue := TQueue.Create;
  Tool := ATool;
  inherited Create(False);
  FreeOnTerminate := True;
end;

destructor TExifQueue.Destroy;
begin
  Event.Free;
  myQueue.Free;
  inherited Destroy;
end;

procedure TExifQueue.Execute;
var
  Item: TExifQueuedJob;
begin
  repeat
    if myQueue.Count = 0 then
      Event.WaitFor(INFINITE);
    Item := TExifQueuedJob(myQueue.Pop);
    if Item <> nil then begin
      Item.Execute;
      Item.Free;
    end;
  until Terminated;
end;

procedure TExifQueue.WakeUp;
begin
  Event.SetEvent;
end;

procedure TExifQueue.Terminate;
begin
  inherited Terminate;
  WakeUp;
end;

procedure TExifQueue.Enqueue(Job: TExifQueuedJob);
begin
  myQueue.Push(Job);
  WakeUp;
end;

{ TExifTool }

constructor TExifTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FError := False;
  FAsyncQueue := TExifQueue.Create(Self);
  OPtions := [poUsePipes];
  {$ifdef windows}
  Executable := 'exiftool.exe';
  {$else}
  Executable := 'exiftool';
  {$endif}
  Parameters.Add('-stay_open');
  Parameters.Add('true');
  Parameters.Add('-@');
  Parameters.Add('-');
  try
    Execute;
  except
    on E:Exception do begin
      WriteLn(E.Message);
      FError := True;
    end;
  end;
end;

destructor TExifTool.Destroy;
begin
  FAsyncQueue.Terminate;
  StayOpenFalse;
  Sleep(100);
  inherited Destroy;
end;

function TExifTool.RunCommand(Command: TStringList): AnsiString;
var
  Prompt: String;
  S: String;
  B: array[0..1000] of Char;
  P: Integer;
  N: Integer;
begin
  Result := '';
  B[0] := #0; // make compiler happy
  if FError then
    exit;
  for S in Command do begin
    S := S + LineEnding;
    Input.Write(S[1], Length(S));
  end;
  S := '-execute' + LineEnding;
  Input.Write(S[1], Length(S));
  Prompt := '{ready}' + LineEnding;
  repeat
    N := Output.Read(B, 1000);
    P := Length(Result);
    SetLength(Result, P + N);
    Move(B, Result[P + 1], N);
  until RightStr(Result, Length(Prompt)) = Prompt;
  Result := LeftStr(Result, Length(Result) - Length(Prompt));
end;

procedure TExifTool.StayOpenFalse;
var
  Cmd: String;
begin
  Cmd := '-stay_open' + LineEnding + 'false' + LineEnding;
  Input.Write(Cmd[1], Length(Cmd));
end;

procedure TExifTool.Enqueue(Job: TExifQueuedJob);
begin
  if FError then
    exit;
  FAsyncQueue.Enqueue(Job);
end;

procedure TExifTool.GetThumbnail(FileName: UTF8String; CB: TDataEvent);
begin
  Enqueue(TExifGetThumbnailJob.Create(self, CB, FileName));
end;

procedure TExifTool.GetTags(FileName: UTF8String; CB: TDataEvent);
begin
  Enqueue(TExifKeywordsParseJob.Create(Self, CB, FileName));
end;

{ TExifGetThumbnailJob }

constructor TExifGetThumbnailJob.Create(ATool: TExifTool; CB: TDataEvent; AFileNama: UTF8String);
begin
  inherited Create(ATool, CB);
  FileName := AFileNama;
end;

procedure TExifGetThumbnailJob.Execute;
var
  Cmd: TStringList;
  JobResult: TStringObject;
begin
  if Tool.Error then
    exit;
  Cmd := TStringList.Create;
  Cmd.Add('-ThumbnailImage');
  Cmd.Add('-b');
  Cmd.Add('-fast');
  Cmd.Add('-fast2');
  Cmd.Add(FileName);
  JobResult := TStringObject.Create;
  JobResult.S := Tool.RunCommand(Cmd);
  Cmd.Free;
  Application.QueueAsyncCall(Callback, LongInt(JobResult));
end;

constructor TExifQueuedJob.Create(ATool: TExifTool; CB: TDataEvent);
begin
  Tool := ATool;
  Callback := CB;
end;

{ TExifKeywordsParseJob }

constructor TExifKeywordsParseJob.Create(ATool: TExifTool; CB: TDataEvent; AFileNama: UTF8String);
begin
  inherited Create(ATool, CB);
  FileName := AFileNama;
end;

procedure TExifKeywordsParseJob.Execute;
var
  Cmd: TStringList;
  Lines: TStringList;
  Tags: TStringList;
  Line: UTF8String;
  Tag: UTF8String;
  P: Integer;
  Data: AnsiString;
begin
  if Tool.Error then
    exit;
  Cmd := TStringList.Create;
  Cmd.Add('-xmp-dc:subject');
  Cmd.Add('-iptc:keywords');
  Cmd.Add('-fast');
  Cmd.Add('-fast2');
  Cmd.Add(FileName);
  Data := Tool.RunCommand(Cmd);
  Cmd.Free;

  Lines := TStringList.Create;
  Tags := TStringList.Create;
  Tags.Sorted := True;
  Lines.Text := Data;
  if Lines.Count > 0 then begin
    for Line in Lines do begin
      P := Pos(':', Line);
      if P > 0 then begin
        Line := RightStr(Line, Length(Line)- P);
        repeat
          P := Pos(',', Line);
          if P > 0 then begin
            Tag := Trim(LeftStr(Line, P-1));
            Line := RightStr(Line, Length(Line) - P);
          end
          else begin
            Tag := Trim(Line);
          end;
          if Tags.IndexOf(Tag) = -1 then begin
            Tags.Add(Tag);
          end;
        until P = 0;
      end;
    end;
  end;
  Lines.Free;
  Application.QueueAsyncCall(Callback, LongInt(Tags));
end;

end.

