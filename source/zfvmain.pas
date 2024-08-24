unit zfvMain;

{$mode objfpc}{$H+}

interface

uses
  LCLVersion,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, Menus, ActnList, ValEdit,
  {$IFDEF ZIPPER}
  zstream, zipper,
  {$ENDIF}
  {$IFDEF ABBREVIA}
  AbArcTyp, AbZipTyp, AbUnzper,
  {$ENDIF}
  MPHexEditor,
  SynEdit, SynEditHighlighter,
  SynHighlighterXML, SynHighlighterCss, SynHighlighterHTML, SynHighlighterJScript,
  SynHighlighterPAS, SynHighlighterLFM,
  SynHighlighterJSON,
  BGRABitmap, BGRABitmapTypes, BGRASVG;

type

  { TMainForm }

  TMainForm = class(TForm)
    acExtractSelected: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    btnLoad: TButton;
    btnExtractSelected: TButton;
    cbFileName: TComboBox;
    ImageList: TImageList;
    lvFiles: TListView;
    MainPanel: TPanel;
    MenuItem1: TMenuItem;
    PageControl: TPageControl;
    PaintBox: TPaintBox;
    LeftPanel: TPanel;
    Panel1: TPanel;
    pnlFileName: TPanel;
    btnBrowse: TSpeedButton;
    FilesPopupMenu: TPopupMenu;
    rbOrigSize: TRadioButton;
    rbScaledSize: TRadioButton;
    Splitter1: TSplitter;
    pgText: TTabSheet;
    pgHex: TTabSheet;
    pgImage: TTabSheet;
    pgInfo: TTabSheet;
    TextViewer: TSynEdit;
    ValueList: TValueListEditor;
    procedure acExtractSelectedExecute(Sender: TObject);
    procedure acExtractSelectedUpdate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure cbFileNameKeyPress(Sender: TObject; var Key: char);
    procedure cbFileNameSelect(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure lvFilesClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure rbSizeChange(Sender: TObject);
  private
    {$IFDEF ZIPPER}
    FUnzipper: TUnzipper;
    {$ENDIF}
    {$IFDEF ABBREVIA}
    FUnzipper: TAbUnzipper;
    {$ENDIF}
    FActivated: Boolean;
    FMaxHistory: Integer;
    FHighlighters: TFPList;
    FHexEditor: TMPHexEditor;
    FImage: TBGRABitmap;
    FSVG: TBGRASvg;
    FOutFileName: String;
    procedure AddToHistory(const AFileName: String);
    procedure DisplayStream(AStream: TStream);

    {$IFDEF ZIPPER}
    procedure CreateOutZipViewerStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoneOutZipViewerStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

    procedure CreateOutZipFileStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoneOutZipFileStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    {$ENDIF}

    function RegisteredHighlighter(AClass: TSynCustomHighlighterClass): TSynCustomHighlighter;

    procedure ClearImage;
    procedure LoadFile(const AFileName: String);
    function ShowImage(AStream: TStream; AExtension: String): Boolean;
    procedure ShowInfo();
    procedure ShowText(AStream: TStream; AExtension: String);
    procedure ShowXML(AStream: TStream);

    function GetIniName: String;
    procedure ReadIni;
    procedure WriteIni;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  StrUtils, LCLType, LCLIntf, IniFiles, TypInfo,
  laz2_xmlread, laz2_xmlwrite, laz2_dom;

const
  APP_NAME = 'Zipped File Viewer';

  FILE_TYPES = 'All files |*.*;*|' +
    'Android package files|*.apk|' +
    'Java archive files|*.jar|' +
    'LibreOffice files|*.odt;*.ods;*.odp;*.odg|' +
    'Microsoft Office files|*.xlsx;*.docx;*.pptx|' +
    'Mozilla Extensions|*.xpi|' +
    'Zip files|*.zip';

  IMG_INDEX_FOLDER = 0;
  IMG_INDEX_TEXT = 1;
  IMG_INDEX_PICTURE = 2;

function GetFixedFontName: String;
var
  idx: Integer;
begin
  Result := Screen.SystemFont.Name;
  idx := Screen.Fonts.IndexOf('Courier New');
  if idx = -1 then
    idx := Screen.Fonts.IndexOf('Courier 10 Pitch');
  if idx = -1 then
    idx := Screen.Fonts.IndexOf('Liberation Mono');
  if idx <> -1 then
    Result := Screen.Fonts[idx]
  else
    for idx := 0 to Screen.Fonts.Count-1 do
      if pos('courier', Lowercase(Screen.Fonts[idx])) = 1 then
      begin
        Result := Screen.Fonts[idx];
        exit;
      end;
end;

function AttributesToStr(Attr: LongWord): String;
begin
  Result := '';
  if Attr and $01 <> 0 then
    Result := Result + ', read-only';
  if Attr and $02 <> 0 then
    Result := result + ', hidden';
  if Attr and $04 <> 0 then
    Result := Result + ', system file';
  if Attr and $08 <> 0 then
    Result := Result + ', volume ID';
  if Attr and $10 <> 0 then
    Result := Result + ', directory';
  if Attr and $02 <> 0 then
    Result := Result + ', archive';
  if Result <> '' then
    Delete(Result, 1, 2);
end;

function BitFlagsToStr(ABitFlags: Word): String;
begin
  Result := '';
  if ABitFlags and %0000000000000001 <> 0 then
    Result := Result + ', encrypted';
  if ABitFlags and %0000000000000110 <> 0 then
    Result := Result + ', compression option';
  if ABitFlags and %000000000001000 <> 0 then
    Result := Result + ', data descriptor';
  if ABitFlags and %0000000000010000 <> 0 then
    Result := Result + ', enhanced deflation';
  if ABitFlags and %0000000000100000 <> 0 then
    Result := Result + ', compressed patched data';
  if ABitFlags and %0000000001000000 <> 0 then
    Result := Result + ', strong encryption';
  if ABitFlags and %0000100000000000 <> 0 then
    Result := Result + ', language encoding';
  if ABitFlags and %0010000000000000 <> 0 then
    Result := Result + ', mask header values';
  if Length(Result) > 0 then
    Delete(Result, 1, 2);
  if Result = '' then
    Result := 'no bit flags';
  Result := Format('%d (%s)', [ABitFlags, Result]);
end;

function CompressionMethodToStr(AMethod: Word): String;
begin
  case AMethod of
    00: Result := 'no compression';
    01: Result := 'shrunk';
    02: Result := 'reduced with compression factor 1';
    03: Result := 'reduced with compression factor 2';
    04: Result := 'reduced with compression factor 3';
    05: Result := 'reduced with compression factor 4';
    06: Result := 'imploded';
    07: Result := 'reserved';
    08: Result := 'deflated';
    09: Result := 'enhanced deflated';
    10: Result := 'PKWare DCL imploded';
    11: Result := 'reserved';
    12: Result := 'compressed using BZIP2';
    13: Result := 'reserved';
    14: Result := 'LZMA';
    15..17: Result := 'reserved';
    18: Result := 'compressed using IBM TERSE';
    19: Result := 'IBM LZ77 z';
    98: Result := 'PPMd version I, Rev 1';
   else Result := 'unknown';
  end;
  Result := Format('%d (%s)', [AMethod, Result]);
end;

function OSToStr(OS: Byte): String;
begin
  {$IFDEF ZIPPER}
  case OS of
    OS_FAT: Result := 'MS-DOS or OS/2 (FAT/VFAT/FAT32)';
    OS_UNIX: Result := 'UNIX';
    OS_OS2: Result := 'OS/2 (HPFS)';
    OS_NTFS: Result := 'NTFS';
    OS_VFAT: Result := 'VFAT';
    OS_OSX: Result := 'OSX';
    else Result := '(unknown)';
  end;
  Result := Format('%d (%s)', [OS, Result]);
  {$ENDIF}
  {$IFDEF ABBREVIA}
  Result := '***';
  {$ENDIF}
end;

{ TMainForm }

procedure TMainForm.AddToHistory(const AFileName: String);
begin
  cbFileName.AddHistoryItem(AFileName, FMaxHistory, true, false);
end;

procedure TMainForm.btnBrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := FILE_TYPES;
      InitialDir := ExtractFilePath(cbFileName.Text);
      if Execute then
      begin
        cbFileName.Items.Insert(0, FileName);
        cbFileName.ItemIndex := 0;
        cbFileName.Text := FileName;
        LoadFile(cbFileName.Text);
      end;
    finally
      Free;
    end;
end;

procedure TMainForm.acExtractSelectedExecute(Sender: TObject);
var
  dlg: TSaveDialog;
  fn: String;
  stream: TMemoryStream;
begin
  if lvFiles.Selected = nil then
    exit;

  fn := lvFiles.Selected.Caption;

  dlg := TSaveDialog.Create(nil);
  try
    dlg.Filter := 'All files|*.*;*';
    dlg.Options := dlg.Options + [ofOverwritePrompt];
    dlg.FileName := ExtractFileName(fn);
    if dlg.Execute then
    begin
      FOutFilename := ExpandFileName(dlg.FileName);
      {$IFDEF ZIPPER}
      FUnzipper.OnCreateStream := @CreateOutZipFileStreamHandler;
      FUnzipper.OnDoneStream := @DoneOutZipFileStreamHandler;
      FUnzipper.UnzipFile(fn);
      {$ENDIF}
      {$IFDEF ABBREVIA}
      stream := TMemoryStream.Create;
      try
        FUnzipper.ExtractToStream(fn, stream);
      finally
        stream.Free;
      end;
      {$ENDIF}
    end;
  finally
    dlg.Free;
  end;
end;

procedure TMainForm.acExtractSelectedUpdate(Sender: TObject);
begin
  acExtractSelected.Enabled := lvFiles.Selected <> nil;
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  LoadFile(cbFileName.Text);
end;

procedure TMainForm.cbFileNameKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    LoadFile(cbFileName.Text);
end;

procedure TMainForm.cbFileNameSelect(Sender: TObject);
begin
  LoadFile(cbFileName.Text);
end;

procedure TMainForm.ClearImage;
begin
  FreeAndNil(FImage);
  Paintbox.Invalidate;
end;

{$IFDEF ZIPPER}
// Handler for unzipping file from archive for viewer:
// creates a stream for unzipped file.
procedure TMainForm.CreateOutZipViewerStreamHandler(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := TMemorystream.Create;
end;

// Handler when unzipping for viewer is finished:
// Show xml stored in the unzipped stream
procedure TMainForm.DoneOutZipViewerStreamHandler(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  DisplayStream(AStream);

  // Destroy the stream created by CreateOutZipStreamHandler
  AStream.Free;
end;

// Handler for unzipping file from archive and saving to file:
// creates a stream for unzipped file.
procedure TMainForm.CreateOutZipFileStreamHandler(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := TFileStream.Create(FOutFileName, fmCreate);
end;

procedure TMainForm.DoneOutZipFileStreamHandler(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream.Free;
end;
{$ENDIF}

procedure TMainForm.DisplayStream(AStream: TStream);
var
  ext: String;
begin
  ext := ExtractFileExt(lvFiles.Selected.Caption);

  // Load into hex viewer.
  AStream.Position := 0;
  FHexEditor.LoadFromStream(AStream);

  // Display info page
  ShowInfo;

  // Try to load as xml
  try
    ShowXML(AStream);
  except
    AStream.Position := 0;
    ShowText(AStream, ext);
  end;

  // Try to load as image
  try
    pgImage.TabVisible := ShowImage(AStream, ext);
  except
    pgImage.TabVisible := false;
  end;
  if not pgImage.TabVisible then
    ClearImage;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    LeftPanel.Constraints.MinWidth := btnExtractSelected.Left + btnExtractSelected.Width;
    btnBrowse.Constraints.MinWidth := btnBrowse.Height + 4;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := APP_NAME;
  PageControl.ActivePageIndex := 0;

  RegisterSvgFormat;

  FHexEditor := TMPHexEditor.Create(self);
  with FHexEditor do
  begin
    Parent := pgHex;
    Align := alClient;
    Font.Name := GetFixedFontName;   // The hard-coded Courier New does not exist in Linux
    Font.Size := 9;
    BytesPerColumn := 1; //IfThen(cbHexSingleBytes.Checked, 1, 2);
    RulerNumberBase := 16; //IfThen(cbHexAddressMode.Checked, 16, 10);
    OffsetFormat := '-!10:$|'; //IfThen(cbHexAddressMode.Checked, '-!10:$|', '-!0A: |');
    ReadOnlyView := true;
//    OnClick := @HexEditorClick;
  end;

  FMaxHistory := 24;
  {$IF LCL_FullVersion >= 2020000}
  cbFileName.TextHint := 'Name of the file to be opened';
  {$ENDIF}
  cbFileName.DropDownCount := FMaxHistory;
  FHighlighters := TFPList.Create;
  TextViewer.Font.Name := GetFixedFontName;
  TextViewer.Font.Quality := fqClearType;
  TextViewer.Font.Size := 9;

  ReadIni;

  if ParamCount > 0 then
    LoadFile(ParamStr(1));
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
  FUnzipper.Free;
  FHighlighters.Free;
  FImage.Free;
  FSVG.Free;
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
begin
  LoadFile(FileNames[0]);
end;

function TMainForm.GetIniName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

procedure TMainForm.LoadFile(const AFileName: String);
var
  i: Integer;
  ext: String;
begin
  // Create unzipper component
  FUnzipper.Free;
  {$IFDEF ZIPPER}
  FUnzipper := TUnzipper.Create;
  {$ENDIF}
  {$IFDEF ABBREVIA}
  FUnzipper := TAbUnzipper.Create(nil);
  {$ENDIF}

  // Load directory contained in zip file into ListView
  try
    FUnzipper.FileName := AFileName;
    {$IFDEF ZIPPER}
    FUnzipper.Examine;
    {$ENDIF}
    lvFiles.Items.BeginUpdate;
    try
      lvFiles.Items.Clear;
      {$IFDEF ZIPPER}
      for i := 0 to FUnzipper.Entries.Count-1 do
      {$ENDIF}
      {$IFDEF ABBREVIA}
      for i := 0 to FUnzipper.Count-1 do
      {$ENDIF}
        with lvFiles.Items.Add do
        begin
          {$IFDEF ZIPPER}
          Data := FUnzipper.Entries[i];
          Caption := FUnzipper.Entries[i].ArchiveFileName;
          {$ENDIF}
          {$IFDEF ABBREVIA}
          Data := FUnzipper.Items[i];
          Caption := FUnzipper.Items[i].FileName;
          {$ENDIF}
          ImageIndex := IMG_INDEX_TEXT;
          if Caption[Length(Caption)] = '/' then
            ImageIndex := IMG_INDEX_FOLDER
          else
          begin
            ext := Lowercase(ExtractFileExt(Caption));
            if TPicture.FindGraphicClassWithFileExt(ext, false) <> nil then
              ImageIndex := IMG_INDEX_PICTURE;
          end;
        end;
      lvFiles.AlphaSort;
    finally
      lvFiles.Items.EndUpdate;
    end;
    AddToHistory(AFileName);
    Caption := Format('%s - [%s]', [APP_NAME, AFileName]);
  except
    i := cbFileName.Items.IndexOf(AFileName);
    if i <> -1 then
      cbFileName.Items.Delete(i);
    raise;
  end;

  TextViewer.Lines.Clear;
  FHexEditor.Clear;
  ClearImage;
  ValueList.Clear;
end;

// Unzip clicked file to a stream as defined by CreateOutZipStreamHandler
procedure TMainForm.lvFilesClick(Sender: TObject);
var
  fn: String;
  stream: TMemoryStream;
begin
  if lvFiles.Selected = nil then
    exit;

  fn := lvFiles.Selected.Caption;
  {$IFDEF ZIPPER}
  FUnzipper.OnCreateStream := @CreateOutZipViewerStreamHandler;
  FUnzipper.OnDoneStream := @DoneOutZipViewerStreamHandler;
  FUnzipper.UnzipFile(fn);
  {$ENDIF}
  {$IFDEF ABBREVIA}
  stream := TMemoryStream.Create;
  try
    FUnzipper.ExtractToStream(fn, stream);
    DisplayStream(stream);
  finally
    stream.Free;
  end;
  {$ENDIF}
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  x, y: Integer;
  stretched: TBGRABitmap;
  imgAspectRatio, paintboxAspectRatio: Double;
  factor: Double;
  w, h: Integer;
  img: TBGRABitmap;
begin
  Paintbox.Canvas.Brush.color := Paintbox.Color;
  Paintbox.Canvas.FillRect(0, 0, Paintbox.Width, Paintbox.Height);

  if FImage = nil then
    exit;

  if FSVG <> nil then
  begin
    if rbOrigSize.Checked then
    begin
      w := round(FSVG.WidthAsPixel);
      h := round(FSVG.HeightAsPixel);
    end else
    begin
      w := Paintbox.Width;
      h := Paintbox.Height;
    end;
    img := TBGRABitmap.Create(w, h);
    try
      FSVG.StretchDraw(img.Canvas2D, taCenter, tlCenter, 0, 0, w, h, false);
      x := (Paintbox.Width - w) div 2;
      if x < 0 then x := 0;
      y := (Paintbox.Height - h) div 2;
      if y < 0 then y := 0;
      img.Draw(Paintbox.Canvas, x, y, false);
    finally
      img.Free;
    end;
  end else
  begin
    if rbOrigSize.checked then
    begin
      x := (Paintbox.Width - FImage.Width) div 2;
      y := (Paintbox.Height - FImage.Height) div 2;
      FImage.Draw(Paintbox.Canvas, x, y, false);
    end else
    begin
      imgAspectRatio := FImage.Height / FImage.Width;
      paintboxAspectRatio := Paintbox.Height / Paintbox.Width;
      if imgAspectRatio > paintboxAspectRatio then
        factor := Paintbox.Height / FImage.Height
      else
        factor := Paintbox.Width / FImage.Width;
      w := round(FImage.Width * factor);
      h := round(FImage.Height * factor);
      x := (Paintbox.Width - w) div 2;
      y := (Paintbox.Height - h) div 2;
      stretched := FImage.Resample(w, h) as TBGRABitmap;
      try
        stretched.Draw(Paintbox.Canvas, x, y, false);
      finally;
        stretched.Free;
      end;
    end;
  end;
end;

procedure TMainForm.rbSizeChange(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TMainForm.ReadIni;
var
  ini: TIniFile;
  T, L, W, H, i: Integer;
  R: TRect;
  List: TStrings;
  s: String;
begin
  ini := TIniFile.Create(GetIniName);
  try
    T := ini.ReadInteger('MainForm', 'Top', Top);
    L := ini.ReadInteger('MainForm', 'Left', Left);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
    if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    SetBounds(L, T, W, H);
    WindowState := wsNormal;
    WindowState := TWindowState(ini.ReadInteger('MainForm', 'WindowState', 0));
    LeftPanel.Width := ini.ReadInteger('MainForm', 'Splitter', LeftPanel.Width);

    FMaxHistory := ini.ReadInteger('History', 'MaxCount', FMaxHistory);
    list := TStringList.Create;
    try
      ini.ReadSection('History', list);
      while list.Count > FMaxHistory do
        list.Delete(list.Count-1);
      cbFileName.Items.Clear;
      for i := 0 to list.Count-1 do
      begin
        s := ini.ReadString('History', list[i], '');
        if (s <> '') and FileExists(s) then
          cbFileName.Items.Add(s);
      end;
    finally
      list.Free;
    end;

  finally
    ini.Free;
  end;
end;

function TMainForm.RegisteredHighlighter(AClass: TSynCustomHighlighterClass): TSynCustomHighlighter;
const
  KEYWORD_COLOR = clNavy;
  COMMENT_COLOR = clTeal;
  STRING_COLOR = clRed;
  DIRECTIVE_COLOR = clRed;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FHighlighters.Count-1 do
    if TSynCustomHighlighter(FHighlighters[i]) is AClass then
    begin
      Result := TSynCustomHighlighter(FHighlighters[i]);
      exit;
    end;
  Result := AClass.Create(self);

  if AClass = TSynXMLSyn then
    with TSynXMLSyn(Result).ProcessingInstructionAttri do
    begin
      Background := clYellow;
    end
  else
  if AClass = TSynFreePascalSyn then
    with TSynFreePascalSyn(Result) do
    begin
      CommentAttri.Foreground := COMMENT_COLOR;
      KeyAttri.Foreground := KEYWORD_COLOR;
      DirectiveAttri.Foreground := DIRECTIVE_COLOR;
      StringAttri.Foreground := STRING_COLOR;
    end
  else
  if AClass = TSynCSSSyn then
    with TSynCSSSyn(Result) do
    begin
      CommentAttri.Foreground := COMMENT_COLOR;
    end
  else
  if AClass = TSynJScriptSyn then
    with TSynJScriptSyn(Result) do
    begin
      CommentAttri.Foreground := COMMENT_COLOR;
      StringAttri.Foreground := STRING_COLOR;
    end;

  FHighlighters.Add(Result);
end;

function TMainForm.ShowImage(AStream: TStream; AExtension: String): Boolean;
var
  x, y: Integer;
begin
  Result := false;

  FreeAndNil(FImage);
  FreeAndNil(FSVG);

  if DetectFileFormat(AStream, AExtension) = ifUnknown then
    exit;

  AStream.Position := 0;
  if SameText(AExtension, '.svg') then
  begin
    FImage := TBGRABitmap.Create(Paintbox.Width, Paintbox.Height);
    FSVG := TBGRASVG.Create(AStream);
  end else
    FImage := TBGRABitmap.Create(AStream);
  Paintbox.Invalidate;
  Result := true;
end;

procedure TMainForm.ShowInfo;
var
  {$IFDEF ZIPPER}
  zipEntry: TFullZipFileEntry;
  {$ENDIF}
  {$IFDEF ABBREVIA}
//  fn: String;
//  idx: Integer;
  zipEntry: TAbZipItem;
  {$ENDIF}
begin
  if lvFiles.ItemIndex = -1 then
    exit;
  {$IFDEF ZIPPER}
  zipEntry := TFullZipFileEntry(lvFiles.Items[lvFiles.ItemIndex].Data);
  {$ENDIF}
  {$IFDEF ABBREVIA}
  {
  fn := lvFiles.Items[lvFiles.ItemIndex].Caption;
  idx := FUnzipper.FindFile(fn);
  zipEntry := FUnzipper.Items[idx];
  }
  zipEntry := TAbZipItem(lvFiles.Items[lvFiles.ItemIndex].Data);
  {$ENDIF};
  if zipEntry = nil then
    exit;

  ValueList.BeginUpdate;
  try
    ValueList.RowCount := 1;

    {$IFDEF ZIPPER}
    ValueList.InsertRow('Archive File Name', zipEntry.ArchiveFileName, true);
    ValueList.InsertRow('  UTF8', zipEntry.UTF8ArchiveFileName, true);
    ValueList.InsertRow('Disk File Name', zipEntry.DiskFileName, true);
    ValueList.InsertRow('  UTF8', zipEntry.UTF8DiskFileName, true);
    ValueList.InsertRow('Date/Time', DateTimeToStr(zipEntry.DateTime), true);
    ValueList.InsertRow('Uncompressed Size', Format('%.0n Bytes', [1.0*zipEntry.Size]), true);
    ValueList.InsertRow('Compressed Size', Format('%.0n Bytes', [1.0*zipEntry.CompressedSize]), true);
    if zipEntry.Size <> 0 then
      ValueList.InsertRow('Compression Ratio', FormatFloat('0%', (1.0-zipEntry.CompressedSize/zipEntry.Size)*100), true);
    ValueList.InsertRow('Compression Method', CompressionMethodToStr(zipEntry.CompressMethod), true);
    ValueList.InsertRow('Compression Level', GetEnumName(TypeInfo(TCompressionLevel), Integer(zipEntry.CompressionLevel)), true);
    ValueList.InsertRow('OS', OSToStr(zipEntry.OS), true);
    ValueList.InsertRow('Attributes', AttributesToStr(zipEntry.Attributes), true);
    ValueList.InsertRow('Bit Flags', BitFlagsToStr(zipEntry.BitFlags), true);
    ValueList.InsertRow('CRC32', Format('$%.8x', [zipEntry.CRC32]), true);
    {$ENDIF}

    {$IFDEF ABBREVIA}
    ValueList.InsertRow('Archive File Name', zipEntry.FileName, true);
    ValueList.InsertRow('Disk File Name', zipEntry.DiskFileName, true);
    ValueList.InsertRow('Raw File Name', zipEntry.RawFileName, true);
    ValueList.InsertRow('Stored Path', zipEntry.StoredPath, true);
    ValueList.InsertRow('Last Modification Date/Time', DateTimeToStr(zipEntry.LastModTimeAsDateTime), true);
    ValueList.InsertRow('Item is encrypted', BoolToStr(zipEntry.IsEncrypted, true), true);
    ValueList.InsertRow('Item is directory', BoolToStr(zipEntry.IsDirectory, true), true);
    ValueList.InsertRow('Compression Method', GetEnumName(TypeInfo(TAbZipCompressionMethod), Integer(zipEntry.CompressionMethod)), true);
    ValueList.InsertRow('Uncompressed Size', Format('%.0n Bytes', [1.0*zipEntry.UncompressedSize]), true);
    ValueList.InsertRow('Compressed Size', Format('%.0n Bytes', [1.0*zipEntry.CompressedSize]), true);
    ValueList.InsertRow('Compression Ratio', FormatFloat('0%', zipEntry.CompressionRatio), true);
    ValueList.InsertRow('CRC32', Format('$%.8x', [zipEntry.CRC32]), true);
    ValueList.InsertRow('Deflation Option', GetEnumName(TypeInfo(TAbZipDeflationOption), Integer(zipEntry.DeflationOption)), true);
    ValueList.InsertRow('Dictionary Size', GetEnumName(TypeInfo(TAbZipDictionarySize), Integer(zipEntry.DictionarySize)), true);
    ValueList.InsertRow('Disk Number Start', IntToStr(zipEntry.DiskNumberStart), true);
    ValueList.InsertRow('External File Attributes', AttributesToStr(zipEntry.ExternalFileAttributes), true);
    ValueList.InsertRow('Extra Field', 'Count: ' + IntToStr(zipEntry.Extrafield.Count), true);
    ValueList.InsertRow('File Comment', zipEntry.FileComment, true);
    ValueList.InsertRow('Host OS', GetEnumName(TypeInfo(TAbZipHostOS), Integer(zipEntry.HostOS)), true);
    ValueList.InsertRow('General-Purpose Bit Flags', BitFlagsToStr(zipEntry.GeneralPurposeBitFlag), true);
    ValueList.InsertRow('LFH Extra Field', 'Count: ' + IntToStr(zipEntry.LFHExtrafield.Count), true);
    ValueList.InsertRow('Native File Attributes', AttributesToStr(zipEntry.NativeFileAttributes), true);
    ValueList.InsertRow('Version Made By', IntToStr(zipEntry.VersionMadeBy), true);
    ValueList.InsertRow('Version Needed To Extract', IntToStr(zipEntry.VersionNeededToExtract), true);
    {$ENDIF}

    ValueList.Row := 1;
  finally
    ValueList.EndUpdate;
  end;
end;

procedure TMainForm.ShowText(AStream: TStream; AExtension: String);
var
  HighlighterClass: TSynCustomHighlighterClass;
begin
  AStream.Position := 0;
  case AExtension of
    '.css':
      HighlighterClass := TSynCssSyn;
    '.html', '.htm':
      HighlighterClass := TSynHTMLSyn;
    '.js':
      HighlighterClass := TSynJScriptSyn;
    '.json':
      HighlighterClass := TSynJSONSyn;
    '.pas', '.pp', '.lpr':
      HighlighterClass := TSynFreePascalSyn;
    '.lfm', '.dfm':
      HighlighterClass := TSynLFMSyn;
    '.xml':
      HighlighterClass := TSynXMLSyn;
    else
      HighlighterClass := nil;
  end;
  if Assigned(HighlighterClass) then
    TextViewer.Highlighter := RegisteredHighlighter(HighlighterClass)
  else
    TextViewer.Highlighter := nil;
  TextViewer.Lines.LoadFromStream(AStream);
end;

// Reads stream into an xml document, improves formatting and displays the
// xml in the SynEdit.
procedure TMainForm.ShowXML(AStream: TStream);
const
  ALLOW_LONG_LINES = false;
//  ALLOW_LONG_LINES = true;
var
  doc: TXMLDocument;
  stream: TStream;
  L: TStrings;
  s, sIndent, sWrap, sTmp, sNew: String;
  i, j, p, nSpc: Integer;
begin
  AStream.Position := 0;
  ReadXMLFile(doc, AStream);

  // Create nice formatting for xml
  stream := TMemoryStream.Create;
  try
    WriteXMLFile(doc, stream);
    // Load nicely formatted xml into SynEdit
    stream.Position := 0;

    if ALLOW_LONG_LINES then
      {%H-}TextViewer.Lines.LoadFromStream(stream)
    else
    begin
      // Wrap attributes of long lines
      L := TStringList.Create;
      try
        L.LoadFromStream(stream);
        for i := L.Count-1 downto 0 do
        begin
          s := L[i];
          if Length(s) <= TextViewer.RightEdge then
            Continue;

          nSpc := 0;
          j := 1;
          while s[j] = ' ' do
          begin
            inc(nSpc);
            inc(j);
          end;
          if s[j] <> '<' then
            continue;
          sIndent := Copy(s, 1, nSpc) + '   ';
          sWrap := '';
          while Length(s) > TextViewer.RightEdge do
          begin
            p := RPos('" ', s);
            if p = 0 then
              Break;
            sNew := Copy(s, p+2, MaxInt);
            sTmp := sNew + ' ' + sWrap;
            if Length(sIndent + sTmp) > TextViewer.RightEdge then
            begin
              if sWrap <> '' then
              begin
                L.Insert(i+1, sIndent + sWrap);
                sWrap := sNew;
              end else
                L.Insert(i+1, sIndent + sNew);
            end else
              sWrap := sTmp;
            s := Copy(s, 1, p);
          end;
          if sWrap <> '' then
            L.Insert(i+1, sIndent + sWrap);
          if Length(s) > TextViewer.RightEdge then
          begin
            p := 1;
            while (p <= Length(s)) and (s[p] = ' ') do
              inc(p);
            while (p <= Length(s)) and (s[p] <> ' ') do
              inc(p);
            while (p <= Length(s)) and (s[p] = ' ') do
              inc(p);
            if p > 1 then
            begin
              sWrap := copy(s, p, MaxInt);
              L.Insert(i+1, sIndent + sWrap);
              s := Copy(L[i], 1, p-1);
            end;
          end;
          L[i] := s;
        end;
        TextViewer.Lines.Assign(L);
      finally
        L.Free;
      end;
    end;
    TextViewer.Highlighter := RegisteredHighlighter(TSynXMLSyn);
  finally
    stream.Free;
    doc.Free;
  end;
end;

procedure TMainForm.WriteIni;
var
  ini: TIniFile;
  i: Integer;
begin
  ini := TIniFile.Create(GetIniName);
  try
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);
    ini.WriteInteger('Position', 'WindowState', Integer(WindowState));
    ini.WriteInteger('MainForm', 'Splitter', LeftPanel.Width);

    ini.WriteInteger('History', 'MaxCount', FMaxHistory);
    ini.EraseSection('History');
    for i := 0 to cbFileName.Items.Count-1 do
      ini.WriteString('History', Format('Item%d', [i+1]), cbFileName.Items[i]);
  finally
    ini.Free;
  end;
end;

end.

