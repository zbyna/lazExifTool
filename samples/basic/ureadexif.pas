unit ureadexif;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, ComCtrls, ValEdit, StdActns, StdCtrls,exiftool,JPEGLib;

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
      pomS:='-xmp:all -j '+MyFileName;
      myArgs.DelimitedText:=pomS;
      ShowMessage(myArgs[1]);  // it is needed to wait until previous jobs done
                               // or use queue
      memLog.Append('Custom command ------');
      memLog.Append(instanceTexifTool.RunCommand(myArgs));
      memLog.Append('---------------------');
      //if ImgData.ProcessFile(MyFileName) then begin
      //  if ImgData.HasEXIF then begin
      //    ValueListEditor1.InsertRow('Camera Make',
      //      ImgData.ExifObj.CameraMake,True);
      //    ValueListEditor1.InsertRow('Camera Modell',
      //      ImgData.ExifObj.CameraModel,True);
      //    ValueListEditor1.InsertRow('Picture DateTime',
      //      FormatDateTime(ISO_DATETIME_FORMAT, ImgData.ExifObj.GetImgDateTime),True);
      //  end
      //  else
      //    ValueListEditor1.InsertRow('No EXIF','No Data',True);
      //end
      //else
      //  ValueListEditor1.InsertRow('No EXIF','Processdata',True);
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



procedure TForm1.Thumbnaildata(Data: PtrInt);
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

end.

