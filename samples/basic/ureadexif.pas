unit ureadexif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, ComCtrls, ValEdit, StdActns, StdCtrls,exiftool,
  fpjson,jsonparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    instanceTexifTool: TExifTool;
    ActionList: TActionList;
    FileExit: TFileExit;
    FileOpen: TFileOpen;
    Image1: TImage;
    ImageList: TImageList;
    memLog: TMemo;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ValueListEditor1: TValueListEditor;
    procedure FileOpenAccept(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure thumbnailData(Data: PtrInt);
    procedure tagsData(Data: PtrInt);
    procedure asyncCommandData(Data: PtrInt);
  public

  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

{ TForm1 }

procedure TForm1.FileOpenAccept(Sender: TObject);
var
  MyFileName,pomS : String;
  myArgs:TStringList;
begin
  MyFileName:= FileOpen.Dialog.FileName;
  if FileExists(MyFileName) then begin
    ValueListEditor1.Clear;
    Image1.Picture.LoadFromFile(MyFileName);
    try
      instanceTexifTool.GetThumbnail(MyFileName, @thumbnailData);
      instanceTexifTool.GetTags(MyFileName, @tagsData);
      myArgs:=TStringList.Create;
      // async custom command line
      pomS:='-xmp:all -j ';  // -Orientation -S
      instanceTexifTool.RunCommandAsync(MyFileName,@asyncCommandData,pomS);
    finally
      myArgs.Free;
    end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  instanceTexifTool.Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  instanceTexifTool:=TExifTool.Create(nil);
  Image1.Proportional:= True;
  Image1.Stretch:= True;
  Image1.AutoSize:=True;
end;



procedure TForm1.thumbnailData(Data: PtrInt);
var
  pomMS:TMemoryStream;
  jpeg: TJPEGImage;
  pomData:TStringObject;
begin
  pomData:=TStringObject(Data);  // to je docela síla :-)
  memLog.Append('GetThumbnail --------');
  memLog.append('String length: '+inttostr(pomData.S.Length));
  pomMS:=TMemoryStream.Create;
  pomMS.Write(pomData.S[1],pomData.S.Length);
  memLog.append('Stream size: '+inttostr(pomMS.Size));
  pomMS.Position := 0;
  jpeg := TJpegImage.Create;
  jpeg.LoadFromStream(pomMS);
  image1.Picture.Assign(jpeg);
  jpeg.Destroy;
  pomMS.Destroy;
  pomData.Destroy;
  memLog.Append('---------------------');
end;

procedure TForm1.tagsData(Data: PtrInt);
var
 pomStringList :Tstringlist;
begin
  memLog.Append('GetTags -------------');
  pomStringList:=TStringList(Data);
  memLog.append(pomStringList.Text);
  memLog.Append('---------------------');
  TStringList(Data).Destroy;  // to je docela síla :-)
end;

procedure TForm1.asyncCommandData(Data: PtrInt);
var
 pomStringObject :TStringObject;
 jData : TJSONData;
 jObject : TJSONObject;
 pomS, pomS2:String;
 i: Integer;
begin
  memLog.Append('Get AsyncCommandData');
  pomStringObject:=TStringObject(Data);
  pomS:=pomStringObject.S;
  memLog.append(pomS);
  memLog.Append('---------------------');
  jData := GetJSON(pomS);
  jObject:=TJSONObject(TJSONArray(jData).Items[0]);
  for i:=0 to jObject.Count-1 do
    begin
      pomS2:=jObject.Names[i];
      ValueListEditor1.InsertRow(pomS2,jObject.Elements[pomS2].AsString,True);
    end;
  jData.Free;
  pomStringObject.Free;
end;

end.

