unit zfvMain;

{$mode objfpc}{$H+}

interface

uses
  LCLVersion,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Buttons, Menus, ActnList,
  zipper,
  MPHexEditor,
  SynEdit, SynEditHighlighter,
  SynHighlighterXML, SynHighlighterCss, SynHighlighterHTML, SynHighlighterJScript,
  SynHighlighterPAS, SynHighlighterLFM,
  SynHighlighterJSON,
  BGRABitmap, BGRABitmapTypes;

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
    pnlFileName: TPanel;
    btnBrowse: TSpeedButton;
    FilesPopupMenu: TPopupMenu;
    Splitter1: TSplitter;
    pgText: TTabSheet;
    pgHex: TTabSheet;
    pgImage: TTabSheet;
    TextViewer: TSynEdit;
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
  private
    FUnzipper: TUnzipper;
    FMaxHistory: Integer;
    FHighlighters: TFPList;
    FHexEditor: TMPHexEditor;
    FImage: TBGRABitmap;
    FOutFileName: String;
    procedure AddToHistory(const AFileName: String);

    procedure CreateOutZipViewerStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoneOutZipViewerStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

    procedure CreateOutZipFileStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoneOutZipFileStreamHandler(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

    function RegisteredHighlighter(AClass: TSynCustomHighlighterClass): TSynCustomHighlighter;

    procedure ClearImage;
    procedure LoadFile(const AFileName: String);
    function ShowImage(AStream: TStream; AExtension: String): Boolean;
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
  StrUtils, LCLType, LCLIntf,
  IniFiles,
  laz2_xmlread, laz2_xmlwrite, laz2_dom,
  BGRASVG;

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
      FUnzipper.OnCreateStream := @CreateOutZipFileStreamHandler;
      FUnzipper.OnDoneStream := @DoneOutZipFileStreamHandler;
      FUnzipper.UnzipFile(fn);
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
var
  ext: String;
begin
  ext := ExtractFileExt(lvFiles.Selected.Caption);

  // Load into hex viewer.
  AStream.Position := 0;
  FHexEditor.LoadFromStream(AStream);

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

procedure TMainForm.FormActivate(Sender: TObject);
begin
  btnBrowse.Constraints.MinWidth := btnBrowse.Height + 8;
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
  FUnzipper := TUnzipper.Create;
  FUnzipper.FileName := AFileName;

  // Load directory contained in zip file into ListView
  try
    FUnzipper.Examine;
    lvFiles.Items.BeginUpdate;
    try
      lvFiles.Items.Clear;
      for i := 0 to FUnzipper.Entries.Count-1 do
        with lvFiles.Items.Add do
        begin
          Caption := FUnzipper.Entries[i].ArchiveFileName;
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
end;

// Unzip clicked file to a stream as defined by CreateOutZipStreamHandler
procedure TMainForm.lvFilesClick(Sender: TObject);
var
  fn: String;
begin
  if lvFiles.Selected = nil then
    exit;

  FUnzipper.OnCreateStream := @CreateOutZipViewerStreamHandler;
  FUnzipper.OnDoneStream := @DoneOutZipViewerStreamHandler;
  fn := lvFiles.Selected.Caption;
  FUnzipper.UnzipFile(fn);
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  x, y: Integer;
begin
  if FImage = nil then
    exit;
  x := (Paintbox.Width - FImage.Width) div 2;
  y := (Paintbox.Height - FImage.Height) div 2;
  FImage.Draw(Paintbox.Canvas, x, y, false);
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
  img: TBgraBitmap;
  x, y: Integer;
  bmp: TBitmap;
begin
  Result := false;

  if DetectFileFormat(AStream, AExtension) = ifUnknown then
    exit;

  AStream.Position := 0;
  FImage.Free;
  FImage := TBGRABitmap.Create(AStream);
  Paintbox.Invalidate;
  Result := true;

  exit;

  img := TBGRABitmap.Create(AStream);
  try
    x := (Paintbox.Width - img.Width) div 2;
    y := (Paintbox.Height - img.Height) div 2;
    if x < 0 then x := 0;
    if y < 0 then y := 0;
    img.Draw(Paintbox.Canvas, x, y, false);
    Result := true;
    (*
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32Bit;
      bmp.SetSize(img.Width, img.Height);
      img.Draw(bmp.Canvas, 0, 0, false);
      ImageViewer.Picture.Assign(bmp);
      Result := true;
    finally
      bmp.Free;
    end;
    *)
  finally
    img.Free;
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

