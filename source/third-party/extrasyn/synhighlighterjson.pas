{-------------------------------------------------------------------------------
SynHighlighterJSON v1.1

This unit provides a SBA JSON highlighter for SynEdit.

(c) Miguel A. Risco-Castillo

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Some concepts in this unit are based in the Original Code of Zhou Kan and
"La Biblia del SynEdit" of Tito Hinostroza.
All Rights Reserved.

v1.1 2019/12/21
Adapted to extrasyn package

v1.0 2016
Initial

-------------------------------------------------------------------------------}

unit SynHighlighterJSON;

// extrasyn.inc is the synedit.inc from laz 1.2.0 synedit package source,
// If it has changed in newer version you might need to copy it again.
// Remember to redclare the syn_lazarus define.
{$I extrasyn.inc}
{$H+}

interface

uses
  SysUtils, Classes, Graphics, SynEditStrConstExtra, SynEditHighlighterFoldBase, SynEditHighlighter, SynEditTypes, SynEditStrConst;

type
//  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull,
//    tkNumber, tkSpace, tkString, tkSymbol, tkAttribute, tkUnknown);

//  TRangeState = (rsUnknown, rsComment);

  TtkTokenKind = (tkKey, tkString, tkReserved, tkNull, tkNumber, tkSpace,
    tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsAttribute, rsObjectValue, rsArrayValue);

  TProcTableProc = procedure of object;

type

  { TSynJSONSyn }

  TSynJSONSyn = class(TSynCustomFoldHighlighter)
  private
    FHLWordsList: TStringList;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: longint;
    fStringLen: integer;
    fToIdent: PChar;
    fTokenPos: integer;
    FTokenID: TtkTokenKind;
    FValueAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighLighterAttributes;
    fKeyAttri: TSynHighLighterAttributes;
    fNumberAttri: TSynHighLighterAttributes;
    fSpaceAttri: TSynHighLighterAttributes;
    fSymbolAttri: TSynHighLighterAttributes;
    fAttribAttri: TSynHighLighterAttributes;
    fDivider:TSynDividerDrawConfigSetting;
    procedure CloseArrayProc;
    procedure CloseObjectProc;
    procedure CommaProc;
    function KeyComp(const aKey: string): boolean;
    procedure OpenArrayProc;
    procedure OpenObjectProc;
    procedure SetHLWordsList(AValue: TStringList);
    procedure SymbolsProc;
    procedure CRProc;
    procedure ColonProc;
    procedure EqualProc;
    procedure EndTokenProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure FoldValidProc;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
      override;
    function GetEOL: boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighLighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    procedure Next; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    function GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting; override;
    property IdentChars;
    class function GetLanguageName: string; override;
  published
    property IdentifierAttri: TSynHighLighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighLighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighLighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighLighterAttributes read fSpaceAttri write fSpaceAttri;
    property SymbolAttri: TSynHighLighterAttributes read fSymbolAttri write fSymbolAttri;
    property AttribAttri: TSynHighLighterAttributes read fAttribAttri write fAttribAttri;
    property ValueAttri: TSynHighLighterAttributes read fValueAttri write fValueAttri;
    property HLWordsList: TStringList read FHLWordsList write SetHLWordsList;
  end;

implementation

var
  Identifiers: array[#0..#255] of bytebool;
  mHashTable: array[#0..#255] of integer;

procedure MakeIdentTable;
var
  I, J: char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else 
        Identifiers[I] := False;
    end;
    J := I; //UpCase(I); In verilog the keywords are in lower case
    case I in ['_', '0'..'9', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J)
        else
          mHashTable[I] := 0;
    end;
  end;
end;

function TSynJSONSyn.KeyComp(const aKey: string): boolean;
var
  I: integer;
  Temp: PChar;
begin
  Temp := fToIdent;   // Test from the second char +1
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do   // Test from the second char := 2
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end 
  else 
    Result := False;
end;

procedure TSynJSONSyn.SetHLWordsList(AValue: TStringList);
begin
  if FHLWordsList=AValue then Exit;
  FHLWordsList:=AValue;
end;

procedure TSynJSONSyn.IdentProc;
var i:integer;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  fStringLen:=Run-fTokenPos;
  fToIdent:=fLine + fTokenPos;
  fTokenID := tkUnknown;
  if FHLWordsList.Count>0 then for i:=0 to FHLWordsList.Count-1 do
  begin
    if KeyComp(FHLWordsList[i]) then fTokenID := tkString;
  end;
  //sba json
  case Upcase(fToIdent^) of
    'A':begin
      if KeyComp('author')
      then fTokenID := tkKey;
    end;
    'D':begin
      if KeyComp('date') or
         KeyComp('description')
      then fTokenID := tkKey;
    end;
    'E':begin
      if KeyComp('explibfiles') or
         KeyComp('expmonolithic') or
         KeyComp('exportpath')
      then fTokenID := tkKey;
    end;
    'F':begin
      if KeyComp('false')
      then fTokenID := tkKey;
    end;
    'I':begin
      if KeyComp('interface') or
         KeyComp('ipcores')
      then fTokenID := tkKey;
    end;
    'L':begin
      if KeyComp('location')
      then fTokenID := tkKey;
    end;
    'N':begin
      if KeyComp('name') or
         KeyComp('null')
      then fTokenID := tkKey;
    end;
    'P':begin
      if KeyComp('portname')
      then fTokenID := tkKey;
      //if KeyComp('process') or
      //   KeyComp('package') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
      //if KeyComp('procedure') then begin fTokenID := tkKey; FoldValidProc; end;
    end;
    'T':begin
      if KeyComp('title') or
         KeyComp('true')
      then fTokenID := tkKey;
    end;
    'U':begin
      if KeyComp('userfiles')
      then fTokenID := tkKey;
    end;
    'V':begin
      if KeyComp('version') then fTokenID := tkKey;
    end;
  end;
end;

procedure TSynJSONSyn.MakeMethodTables;
var
  I: char;
begin
  for I := #0 to #255 do
    case I of
      #0 : fProcTable[I]      := @NullProc;
      '#','$','&','@' : fProcTable[I]  := @SymbolsProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      '{': fProcTable[I]      := @OpenObjectProc;
      '}': fProcTable[I]      := @CloseObjectProc;
      '[': fProcTable[I]      := @OpenArrayProc;
      ']': fProcTable[I]      := @CloseArrayProc;
      ',': fProcTable[I]      := @CommaProc;
      ':': fProcTable[I]      := @ColonProc;
      '=': fProcTable[I]      := @EqualProc;
      '>': fProcTable[I]      := @GreaterProc;
      #10: fProcTable[I]      := @LFProc;
      #13: fProcTable[I]      := @CRProc;
      '<': fProcTable[I]      := @LowerProc;
      '%': fProcTable[I]      := @ModSymbolProc;
      '!': fProcTable[I]      := @NotSymbolProc;
      '0'..'9','-': fProcTable[I] := @NumberProc;
      '|': fProcTable[I]      := @OrSymbolProc;
      '+': fProcTable[I]      := @PlusProc;
      '*': fProcTable[I]      := @StarProc;
      '/': fProcTable[I]      := @SlashProc;
      '.': fProcTable[I]      := @PointProc;
      ')': fProcTable[I]      := @RoundCloseProc;
      '(': fProcTable[I]      := @RoundOpenProc;
      ';': fProcTable[I]      := @SemiColonProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      '"': fProcTable[I]      := @StringProc;     //" #34
      else
        fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynJSONSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := clWindowText;
  AddAttribute(fIdentifierAttri);
  
  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  AddAttribute(fKeyAttri);
  
  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clRed;
  AddAttribute(fNumberAttri);
  
  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrWhitespace);
  AddAttribute(fSpaceAttri);
  
  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clTeal;
  AddAttribute(fSymbolAttri);
  
  fAttribAttri := TSynHighLighterAttributes.Create(SYNS_AttrAttributeName);
  fAttribAttri.Foreground := clPurple; // clMaroon;
  AddAttribute(fAttribAttri);

  FValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue);
  FValueAttri.Foreground := clNavy;
  AddAttribute(FValueAttri);

  SetAttributesOnChange(@DefHighlightChange);
  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterJSON;

  fDivider.Color:=clNone;
  FHLWordsList:=TStringList.Create;
end;

destructor TSynJSONSyn.Destroy;
begin
  FreeAndNil(FHLWordsList);
  inherited Destroy;
end;

procedure TSynJSONSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLine       := PChar(NewValue);
  Run         := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynJSONSyn.CloseArrayProc;
begin
  SymbolsProc;
  FRange := rsUnknown;
end;

procedure TSynJSONSyn.CloseObjectProc;
begin
  SymbolsProc;
  FRange := rsUnknown;
end;

procedure TSynJSONSyn.ColonProc;
begin
  SymbolsProc;
  FRange := rsObjectValue;
end;

procedure TSynJSONSyn.CommaProc;
begin
  SymbolsProc;
  if FRange = rsObjectValue then
    FRange := rsAttribute;
end;

procedure TSynJSONSyn.SymbolsProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynJSONSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynJSONSyn.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.GreaterProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynJSONSyn.LowerProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.MinusProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.SlashProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynJSONSyn.ModSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynJSONSyn.NotSymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynJSONSyn.NumberProc;
begin
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9','.','-','+','e','E'] do
    inc(Run);
end;

procedure TSynJSONSyn.OpenArrayProc;
begin
  SymbolsProc;
  FRange := rsArrayValue;
end;

procedure TSynJSONSyn.OpenObjectProc;
begin
  SymbolsProc;
  FRange := rsAttribute;
end;

procedure TSynJSONSyn.OrSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynJSONSyn.PlusProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.PointProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynJSONSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynJSONSyn.StarProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynJSONSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynJSONSyn.EndTokenProc;
begin
  fTokenID := tkKey;
  EndCodeFoldBlock();
end;

procedure TSynJSONSyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynJSONSyn.FoldValidProc;
var
  tmp:integer;
  found:boolean;
begin
  tmp:=run;
  found:=false;
  while (not (fLine[tmp] in [#0, #10, #13])) do if fLine[tmp]=';' then
  begin
    found:=true;
    break;
  end else inc(tmp);
  if not found then StartCodeFoldBlock(nil);
end;

procedure TSynJSONSyn.Next;
begin
  fTokenPos := Run;
//  if fRange = rsComment then CommentProc else
    fProcTable[fLine[Run]];
end;

function TSynJSONSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result    := fKeyAttri;
    SYN_ATTR_STRING: Result     := fValueAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result     := fSymbolAttri;
    else
      Result := nil;
  end;
end;

function TSynJSONSyn.GetEOL: boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynJSONSyn.GetToken: string;
var
  Len: longint;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynJSONSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart := fLine + fTokenPos;
end;

function TSynJSONSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynJSONSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkString:
      if FRange in [rsObjectValue, rsArrayValue] then
        Result := FValueAttri
      else
        Result := fAttribAttri;
    tkKey: Result        := fKeyAttri;
    tkNumber: Result     := fNumberAttri;
    tkSpace: Result      := fSpaceAttri;
    tkSymbol: Result     := fSymbolAttri;
    tkUnknown: Result    := fIdentifierAttri;
    else 
      Result := nil;
  end;
end;

function TSynJSONSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynJSONSyn.GetTokenPos: integer;
begin
  Result := fTokenPos;
end;

function TSynJSONSyn.GetRange: Pointer;
begin
//  Result := Pointer(fRange);
  CodeFoldRange.RangeType := Pointer(PtrInt(fRange));
  Result := inherited;
end;

procedure TSynJSONSyn.ReSetRange;
begin
  inherited;
  fRange := rsUnknown;
end;

function TSynJSONSyn.GetDrawDivider(Index: integer
  ): TSynDividerDrawConfigSetting;
begin
  Result:=fDivider;
end;

procedure TSynJSONSyn.SetRange(Value: Pointer);
begin
  //  fRange := TRangeState(Value);
  inherited;
  fRange := TRangeState(PtrUInt(CodeFoldRange.RangeType));
end;

function TSynJSONSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

class function TSynJSONSyn.GetLanguageName :string;
begin
  Result := SYNS_LangJSON;
end;

function TSynJSONSyn.GetSampleSource: string;
begin
  Result :=
    '{"menu": {'+LineEnding+
    '  "id": "file",'+LineEnding+
    '  "value": "File",'+LineEnding+
    '  "popup": {'+LineEnding+
    '    "menuitem": ['+LineEnding+
    '      {"value": "New", "onclick": "CreateNewDoc()"},'+LineEnding+
    '      {"value": "Open", "onclick": "OpenDoc()"},'+LineEnding+
    '      {"value": "Close", "onclick": "CloseDoc()"}'+LineEnding+
    '    ]'+LineEnding+
    '  }'+LineEnding+
    '}}';
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynJSONSyn);

end.

