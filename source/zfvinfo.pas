unit zfvInfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ValEdit,
  {$IFDEF ZIPPER}
  zstream, zipper;
  {$ENDIF}
  {$IFDEF ABBREVIA}
  AbArcTyp, AbZipTyp, AbUnzper;
  {$ENDIF}

type

  { TInfoForm }

  TInfoForm = class(TForm)
    ButtonPanel: TButtonPanel;
    ValueList: TValueListEditor;
    procedure FormActivate(Sender: TObject);
  private
    FActivated: Boolean;
  {$IFDEF ZIPPER}
  private
    FEntry: TFullZipFileEntry;
    procedure SetEntry(AValue: TFullZipFileEntry);
  public
    property ZipFileEntry: TFullZipFileEntry read FEntry write SetEntry;
  {$ENDIF}
  {$IFDEF ABBREVIA}
  private
    FEntry: TAbZipItem;
    procedure SetEntry(AValue: TAbZipItem);
  public
    property ZipFileEntry: TAbZipItem read FEntry write SetEntry;
  {$ENDIF}
  end;



var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

uses
  TypInfo;

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
  if ABitFlags and %0000000000000100 <> 0 then
    Result := Result + ', data descriptor';
  if ABitFlags and %0000000000001000 <> 0 then
    Result := Result + ', enhanced deflation';
  if ABitFlags and %0000000000010000 <> 0 then
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

procedure TInfoForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    Height := ValueList.RowCount * ValueList.DefaultRowHeight + ValueList.GridLineWidth * 3 +
      ValueList.BorderSpacing.Around*2 +
      ButtonPanel.Height + ButtonPanel.BorderSpacing.Around;
    FActivated := true;
  end;
end;

{$IFDEF ZIPPER}
procedure TInfoForm.SetEntry(AValue: TFullZipFileEntry);
{$ENDIF}
{$IFDEF ABBREVIA}
procedure TInfoForm.SetEntry(AValue: TAbZipItem);
{$ENDIF}
begin
  if AValue = FEntry then
    exit;

  FEntry := AValue;
  ValueList.BeginUpdate;
  try
    ValueList.RowCount := 1;

    {$IFDEF ZIPPER}
    ValueList.InsertRow('Archive File Name', FEntry.ArchiveFileName, true);
    ValueList.InsertRow('  UTF8', FEntry.UTF8ArchiveFileName, true);
    ValueList.InsertRow('Disk File Name', FEntry.DiskFileName, true);
    ValueList.InsertRow('  UTF8', FEntry.UTF8DiskFileName, true);
    ValueList.InsertRow('', '', true);
    ValueList.InsertRow('Date/Time', DateTimeToStr(FEntry.DateTime), true);
    ValueList.InsertRow('Size', Format('%.0n Bytes', [1.0*FEntry.Size]), true);
    ValueList.InsertRow('Compressed Size', Format('%.0n Bytes', [1.0*FEntry.CompressedSize]), true);
    if FEntry.Size <> 0 then
      ValueList.InsertRow('Compression Ratio', FormatFloat('0%', (1.0-FEntry.CompressedSize/FEntry.Size)*100), true);
    ValueList.InsertRow('Compression Method', CompressionMethodToStr(FEntry.CompressMethod), true);
    ValueList.InsertRow('Compression Level', GetEnumName(TypeInfo(TCompressionLevel), Integer(FEntry.CompressionLevel)), true);
    ValueList.InsertRow('', '', true);
    ValueList.InsertRow('OS', OSToStr(FEntry.OS), true);
    ValueList.InsertRow('Attributes', AttributesToStr(FEntry.Attributes), true);
    ValueList.InsertRow('Bit Flags', BitFlagsToStr(FEntry.BitFlags), true);
    ValueList.InsertRow('CRC32', Format('$%.8x', [FEntry.CRC32]), true);
    {$ENDIF}

    {$IFDEF ABBREVIA}
    ValueList.InsertRow('Archive File Name', FEntry.FileName, true);
    ValueList.InsertRow('Disk File Name', FEntry.DiskFileName, true);
    ValueList.InsertRow('Raw File Name', FEntry.RawFileName, true);
    ValueList.InsertRow('Stored Path', FEntry.StoredPath, true);
    ValueList.InsertRow('Last Modification Date/Time', DateTimeToStr(FEntry.LastModTimeAsDateTime), true);
    ValueList.InsertRow('Encrypted?', BoolToStr(FEntry.IsEncrypted, true), true);
    ValueList.InsertRow('Directory?', BoolToStr(FEntry.IsDirectory, true), true);

    ValueList.InsertRow('Compression Method', GetEnumName(TypeInfo(TAbZipCompressionMethod), Integer(FEntry.CompressionMethod)), true);
    ValueList.InsertRow('Uncompressed Size', Format('%.0n Bytes', [1.0*FEntry.UncompressedSize]), true);
    ValueList.InsertRow('Compressed Size', Format('%.0n Bytes', [1.0*FEntry.CompressedSize]), true);
    ValueList.InsertRow('Compression Ratio', FormatFloat('0%', FEntry.CompressionRatio), true);
    ValueList.InsertRow('CRC32', Format('$%.8x', [FEntry.CRC32]), true);
    ValueList.InsertRow('Deflation Option', GetEnumName(TypeInfo(TAbZipDeflationOption), Integer(FEntry.DeflationOption)), true);
    ValueList.InsertRow('Dictionary Size', GetEnumName(TypeInfo(TAbZipDictionarySize), Integer(FEntry.DictionarySize)), true);
    ValueList.InsertRow('Disk Number Start', IntToStr(FEntry.DiskNumberStart), true);
    ValueList.InsertRow('External File Attributes', AttributesToStr(FEntry.ExternalFileAttributes), true);
    ValueList.InsertRow('Extra Field', 'Count: ' + IntToStr(FEntry.Extrafield.Count), true);
    ValueList.InsertRow('File Comment', FEntry.FileComment, true);
    ValueList.InsertRow('Host OS', GetEnumName(TypeInfo(TAbZipHostOS), Integer(FEntry.HostOS)), true);
    ValueList.InsertRow('General-Purpose Bit Flags', BitFlagsToStr(FEntry.GeneralPurposeBitFlag), true);
    ValueList.InsertRow('LFH Extra Field', 'Count: ' + IntToStr(FEntry.LFHExtrafield.Count), true);
    ValueList.InsertRow('Native File Attributes', AttributesToStr(FEntry.NativeFileAttributes), true);
    ValueList.InsertRow('Version Made By', IntToStr(FEntry.VersionMadeBy), true);
    ValueList.InsertRow('Version Needed To Extract', IntToStr(FEntry.VersionNeededToExtract), true);

    {$ENDIF}

    ValueList.Row := 1;
  finally
    ValueList.EndUpdate;
  end;
end;

end.

