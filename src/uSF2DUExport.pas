(* C2PP
  ***************************************************************************

  SVG folder to Delphi unit

  Copyright 2024-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  This program is designed for Delphi developers wishing to use vector
  images (in SVG format) into their projects.

  It generates a unit from the list of SVG files contained in a folder.

  An SVG list is created as an array of multilines strings containing the
  textual source code of each vector image.

  Constants and an enumeration containing the unit name and the SVG file
  names provide the indices of the associated source code in the list. This
  makes it easier to find your way around and manage several SVG lists in a
  single project.

  You can use the result in a VCL or FireMonkey Delphi project, with Skia
  enabled (via Skia4Delphi project or natively) and the unit
  Olf.Skia.SVGToBitmap.pas from
  https://github.com/DeveloppeurPascal/librairies/

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://svgfolder2delphiunit.olfsoftware.fr/

  Project site :
  https://github.com/DeveloppeurPascal/SVGFolder2DelphiUnit

  ***************************************************************************
  File last update : 2025-07-13T16:02:14.000+02:00
  Signature : d0075d7cec2241d979ec5a712235a2df1f797e66
  ***************************************************************************
*)

unit uSF2DUExport;

interface

uses
  System.Types;

procedure ExportFoldersToPascalUnit(const SVGFolders: TStringDynArray;
  const ToUnitFilePath, TabName: string; const UseMultilineString,
  GenerateOlfSkiaSVGToBitmapCode: boolean);

function OnlyChar(const S: string; const AddUnderscoreAsFirstCharacterIfNumber
  : boolean = false): string;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.Character,
  System.SysUtils,
  uConsts,
  System.DateUtils;

function GetRelativePath(const ForPath, CurrentPath: string): string;
var
  i, j: integer;
begin
  i := 0;
  j := 0;
  while (i < length(ForPath)) and (i < length(CurrentPath)) and
    (ForPath.Chars[i] = CurrentPath.Chars[i]) do
  begin
    if ForPath.Chars[i] = tpath.DirectorySeparatorChar then
      j := i;
    inc(i);
  end;
  if (i < length(ForPath)) or ((i < length(CurrentPath)) and
    (CurrentPath.Chars[i] <> tpath.DirectorySeparatorChar)) then
  begin
    result := ForPath.Substring(j + 1);
    i := j + 1;
  end
  else
    result := '';
  while (i < length(CurrentPath)) do
  begin
    if CurrentPath.Chars[i] = tpath.DirectorySeparatorChar then
      if result.IsEmpty then
        result := '.' + tpath.DirectorySeparatorChar
      else if result = '.' + tpath.DirectorySeparatorChar then
        result := '.' + result
      else
        result := '..' + tpath.DirectorySeparatorChar + result;
    inc(i);
  end;
end;

function AddSpace(const Nb: cardinal): string;
var
  i: cardinal;
begin
  result := '';
  for i := 1 to Nb do
    result := result + ' ';
end;

function OnlyChar(const S: string; const AddUnderscoreAsFirstCharacterIfNumber
  : boolean): string;
var
  c: char;
  i: integer;
  Upper: boolean;
begin
  result := '';
  Upper := true;
  for i := 0 to length(S) - 1 do
  begin
    c := S.Chars[i];

    if c.IsInArray(['-', '_', '.']) then
      Upper := true
    else if charinset(c, ['0' .. '9', 'a' .. 'z', 'A' .. 'Z']) then
      if Upper then
      begin
        Upper := false;
        result := result + UpperCase(c);
      end
      else
        result := result + c;
  end;
  if AddUnderscoreAsFirstCharacterIfNumber and result.Chars[0].IsDigit then
    result := '_' + result;
end;

function getConstantName(Const FileName: string): string;
begin
  result := 'CSVG' + OnlyChar(tpath.GetFileNameWithoutExtension(FileName));
end;

procedure AjouteSVGSource(Const FileName: string; var Destination: TStringList;
  const CopyAsPascalStrings: boolean = false);
const
  CMaxMultiLineLength = 1000;
  CMaxLineLength = 128;
var
  SVGSource: TStringDynArray;
  i, j: integer;
  first: boolean;
  S: string;
  CharPos: integer;
begin
  first := true;
  SVGSource := tfile.ReadAllLines(FileName);
  for i := 0 to length(SVGSource) - 1 do
    if CopyAsPascalStrings then
    begin // "old" strings
      j := 0;
      while j < SVGSource[i].length do
      begin
        S := SVGSource[i].Substring(j, CMaxLineLength);
        if not S.IsEmpty then
          if first then
          begin
            Destination.add(AddSpace(2) + '''' + S.Replace('''',
              '''''') + '''');
            first := false;
          end
          else
            Destination.add(AddSpace(2) + '+ ''' + S.Replace('''',
              '''''') + '''');
        inc(j, CMaxLineLength);
      end
    end
    else
    begin // multiline strings
      // replace a SPACE by a NEW LINE
      if (SVGSource[i].length > CMaxMultiLineLength) then
      begin
        SVGSource[i] := SVGSource[i].trim;
        CharPos := SVGSource[i].LastIndexOf(' ', CMaxMultiLineLength);
        while (CharPos > -1) and (SVGSource[i].length > CMaxMultiLineLength) do
        begin
          Destination.add(SVGSource[i].Substring(0, CharPos + 1).trim);
          SVGSource[i] := SVGSource[i].Substring(CharPos + 1).trim;
          CharPos := SVGSource[i].LastIndexOf(' ', CMaxMultiLineLength);
        end;
      end;
      // replace a COMMA by a NEW LINE
      if (SVGSource[i].length > CMaxMultiLineLength) then
      begin
        SVGSource[i] := SVGSource[i].trim;
        CharPos := SVGSource[i].LastIndexOf(',', CMaxMultiLineLength);
        while (CharPos > -1) and (SVGSource[i].length > CMaxMultiLineLength) do
        begin
          Destination.add(SVGSource[i].Substring(0, CharPos + 1).trim);
          SVGSource[i] := SVGSource[i].Substring(CharPos + 1).trim;
          CharPos := SVGSource[i].LastIndexOf(',', CMaxMultiLineLength);
        end;
      end;
      // add the end of the line
      Destination.add(SVGSource[i]);
    end;
end;

procedure ExportFoldersToPascalUnit(const SVGFolders: TStringDynArray;
  const ToUnitFilePath, TabName: string; const UseMultilineString,
  GenerateOlfSkiaSVGToBitmapCode: boolean);
var
  Files: TStringDynArray;
  SVGList: TStringList;
  i, j: integer;
  DestinationUnit: TStringList;
  UnitFileName: string;
begin
  if (length(SVGFolders) < 1) or ToUnitFilePath.IsEmpty then
    exit;

  SVGList := TStringList.Create;
  try
    for i := 0 to length(SVGFolders) - 1 do
    begin
      Files := tdirectory.GetFiles(SVGFolders[i]);
      for j := 0 to length(Files) - 1 do
        if Files[j].ToLower.EndsWith('.svg') then
          SVGList.add(Files[j]);
    end;

    UnitFileName := OnlyChar(tpath.GetFileNameWithoutExtension
      (ToUnitFilePath), true);

    if (SVGList.Count > 0) then
    begin
      SVGList.Sort;
      DestinationUnit := TStringList.Create;
      try
        DestinationUnit.add('unit ' + UnitFileName + ';');
        DestinationUnit.add('');
        DestinationUnit.add('// ****************************************');
        DestinationUnit.add('// * SVG from folder :');
        for i := 0 to length(SVGFolders) - 1 do
          DestinationUnit.add('// * ' + GetRelativePath(SVGFolders[i],
            ToUnitFilePath));
        DestinationUnit.add('// ****************************************');
        DestinationUnit.add('//');
        DestinationUnit.add('// This file contains a list of constants and');
        DestinationUnit.add('// an enumeration to access to SVG source codes');
        DestinationUnit.add('// from the generated array of strings.');
        DestinationUnit.add('//');
        DestinationUnit.add('// ****************************************');
        DestinationUnit.add('// File generator : ' + CAboutTitle + ' v' +
          CAboutVersionNumber);
        DestinationUnit.add('// Website : ' + CAboutURL);
        DestinationUnit.add('// Generation date : ' + DateToISO8601(now, true));
        DestinationUnit.add('//');
        DestinationUnit.add('// Don''t do any change on this file.');
        DestinationUnit.add('// They will be erased by next generation !');
        DestinationUnit.add('// ****************************************');
        DestinationUnit.add('');
        DestinationUnit.add('interface');
        DestinationUnit.add('');
        if GenerateOlfSkiaSVGToBitmapCode then
        begin
          DestinationUnit.add('{$IF Defined(FRAMEWORK_VCL)}');
          DestinationUnit.add('');
          DestinationUnit.add('uses');
          DestinationUnit.add(AddSpace(2) + 'VCL.Graphics;');
          DestinationUnit.add('{$ELSE IF Defined(FRAMEWORK_FMX)}');
          DestinationUnit.add('');
          DestinationUnit.add('uses');
          DestinationUnit.add(AddSpace(2) + 'FMX.Graphics;');
          DestinationUnit.add('{$ELSE}');
          DestinationUnit.add
            ('{$MESSAGE FATAL ''Is it a VCL or FMX program ?''}');
          DestinationUnit.add('{$ENDIF}');
          DestinationUnit.add('');
        end;
        DestinationUnit.add('const');
        for i := 0 to SVGList.Count - 1 do
          DestinationUnit.add(AddSpace(2) + getConstantName(SVGList[i]) + ' = '
            + i.ToString + ';');
        DestinationUnit.add('');
        DestinationUnit.add('type');
        DestinationUnit.add('{$SCOPEDENUMS ON}');
        DestinationUnit.add(AddSpace(2) + 'T' + TabName + 'Index = (');
        for i := 0 to SVGList.Count - 1 do
          if (i < SVGList.Count - 1) then
            DestinationUnit.add(AddSpace(4) +
              OnlyChar(tpath.GetFileNameWithoutExtension(SVGList[i]), true) +
              ' = ' + getConstantName(SVGList[i]) + ',')
          else
            DestinationUnit.add(AddSpace(4) +
              OnlyChar(tpath.GetFileNameWithoutExtension(SVGList[i]), true) +
              ' = ' + getConstantName(SVGList[i]) + ');');
        DestinationUnit.add('');
        DestinationUnit.add(AddSpace(2) + 'T' + TabName + ' = class');
        DestinationUnit.add(AddSpace(2) + 'private');
        DestinationUnit.add(AddSpace(2) + 'class var');
        DestinationUnit.add(AddSpace(4) + 'FTag: integer;');
        DestinationUnit.add(AddSpace(4) + 'FTagBool: Boolean;');
        DestinationUnit.add(AddSpace(4) + 'FTagFloat: Single;');
        DestinationUnit.add(AddSpace(4) + 'FTagObject: TObject;');
        DestinationUnit.add(AddSpace(4) + 'FTagString: string;');
        if GenerateOlfSkiaSVGToBitmapCode then
          DestinationUnit.add(AddSpace(4) + 'FBitmapListIndex: integer;');
        DestinationUnit.add(AddSpace(4) +
          'class procedure SetTag(const Value: integer); static;');
        DestinationUnit.add(AddSpace(4) +
          'class procedure SetTagBool(const Value: Boolean); static;');
        DestinationUnit.add(AddSpace(4) +
          'class procedure SetTagFloat(const Value: Single); static;');
        DestinationUnit.add(AddSpace(4) +
          'class procedure SetTagObject(const Value: TObject); static;');
        DestinationUnit.add(AddSpace(4) +
          'class procedure SetTagString(const Value: string); static;');
        DestinationUnit.add(AddSpace(2) + 'public const');
        for i := 0 to SVGList.Count - 1 do
          DestinationUnit.add(AddSpace(4) +
            OnlyChar(tpath.GetFileNameWithoutExtension(SVGList[i]), true) +
            ' = ' + getConstantName(SVGList[i]) + ';');
        DestinationUnit.add(AddSpace(4) +
          'class property Tag: integer read FTag write SetTag;');
        DestinationUnit.add(AddSpace(4) +
          'class property TagBool: Boolean read FTagBool write SetTagBool;');
        DestinationUnit.add(AddSpace(4) +
          'class property TagFloat: Single read FTagFloat write SetTagFloat;');
        DestinationUnit.add(AddSpace(4) +
          'class property TagObject: TObject read FTagObject write SetTagObject;');
        DestinationUnit.add(AddSpace(4) +
          'class property TagString: string read FTagString write SetTagString;');
        DestinationUnit.add(AddSpace(4) +
          'class function SVG(const Index: Integer): string; overload;');
        DestinationUnit.add(AddSpace(4) + 'class function SVG(const Index: T' +
          TabName + 'Index) : string; overload;');
        DestinationUnit.add(AddSpace(4) + 'class function Count : Integer;');
        DestinationUnit.add(AddSpace(4) + 'class constructor Create;');
        if GenerateOlfSkiaSVGToBitmapCode then
        begin
          DestinationUnit.add(AddSpace(4) +
            'class function Bitmap(const Index: T' + TabName + 'Index;');
          DestinationUnit.add(AddSpace(6) +
            'const width, height: single; const BitmapScale: single): TBitmap;');
        end;
        DestinationUnit.add(AddSpace(2) + 'end;');
        DestinationUnit.add('');
        DestinationUnit.add('var');
        DestinationUnit.add(AddSpace(2) + TabName + ' : array of String;');
        DestinationUnit.add('');
        if GenerateOlfSkiaSVGToBitmapCode then
        begin
          DestinationUnit.add('/// <summary>');
          DestinationUnit.add('/// Returns a bitmap from a T' + TabName +
            ' SVG file');
          DestinationUnit.add('/// </summary>');
          DestinationUnit.add('function getBitmapFromSVG(const Index: T' +
            TabName + 'Index;');
          DestinationUnit.add
            ('  const width, height: single; const BitmapScale: single): tbitmap; overload;');
          DestinationUnit.add('');
        end;
        DestinationUnit.add('implementation');
        DestinationUnit.add('');
        DestinationUnit.add('uses');
        if GenerateOlfSkiaSVGToBitmapCode then
          DestinationUnit.add(AddSpace(2) + 'Olf.Skia.SVGToBitmap,');
        DestinationUnit.add(AddSpace(2) + 'System.SysUtils;');
        DestinationUnit.add('');
        DestinationUnit.add('{ T' + TabName + ' }');
        DestinationUnit.add('');
        DestinationUnit.add('class constructor T' + TabName + '.Create;');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'inherited;');
        if GenerateOlfSkiaSVGToBitmapCode then
          DestinationUnit.add(AddSpace(2) + 'FBitmapListIndex := 0;');
        DestinationUnit.add(AddSpace(2) + 'FTag := 0;');
        DestinationUnit.add(AddSpace(2) + 'FTagBool := false;');
        DestinationUnit.add(AddSpace(2) + 'FTagFloat := 0;');
        DestinationUnit.add(AddSpace(2) + 'FTagObject := nil;');
        DestinationUnit.add(AddSpace(2) + 'FTagString := '''';');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class procedure T' + TabName +
          '.SetTag(const Value: integer);');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'FTag := Value;');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class procedure T' + TabName +
          '.SetTagBool(const Value: Boolean);');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'FTagBool := Value;');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class procedure T' + TabName +
          '.SetTagFloat(const Value: Single);');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'FTagFloat := Value;');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class procedure T' + TabName +
          '.SetTagObject(const Value: TObject);');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'FTagObject := Value;');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class procedure T' + TabName +
          '.SetTagString(const Value: string);');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'FTagString := Value;');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class function T' + TabName +
          '.SVG(const Index: Integer): string;');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'if (index < Count) then');
        DestinationUnit.add(AddSpace(2) + '  result := ' + TabName + '[index]');
        DestinationUnit.add(AddSpace(2) + 'else');
        DestinationUnit.add(AddSpace(2) +
          '  raise Exception.Create(''SVG not found. Index out of range.'');');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class function T' + TabName +
          '.SVG(const Index : T' + TabName + 'Index): string;');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'result := SVG(ord(index));');
        DestinationUnit.add('end;');
        DestinationUnit.add('');
        DestinationUnit.add('class function T' + TabName + '.Count: Integer;');
        DestinationUnit.add('begin');
        DestinationUnit.add(AddSpace(2) + 'result := length(' + TabName + ');');
        DestinationUnit.add('end;');
        if GenerateOlfSkiaSVGToBitmapCode then
        begin
          DestinationUnit.add('');
          DestinationUnit.add('class function T' + TabName +
            '.Bitmap(const Index: T' + TabName + 'Index;');
          DestinationUnit.add(AddSpace(2) +
            'const width, height: single; const BitmapScale: single): TBitmap;');
          DestinationUnit.add('begin');
          DestinationUnit.add(AddSpace(2) +
            'result := TOlfSVGBitmapList.Bitmap(ord(Index) + T' + TabName +
            '.FBitmapListIndex,');
          DestinationUnit.add(AddSpace(4) +
            'round(width), round(height), BitmapScale);');
          DestinationUnit.add('end;');
          DestinationUnit.add('');
          DestinationUnit.add('procedure RegisterSVGBitmap;');
          DestinationUnit.add('begin');
          DestinationUnit.add(AddSpace(2) + 'T' + TabName +
            '.FBitmapListIndex := TOlfSVGBitmapList.AddItem(' + TabName + ');');
          DestinationUnit.add('end;');
          DestinationUnit.add('');
          DestinationUnit.add('function getBitmapFromSVG(const Index: T' +
            TabName + 'Index;');
          DestinationUnit.add
            ('  const width, height: single; const BitmapScale: single): tbitmap; overload;');
          DestinationUnit.add('begin');
          DestinationUnit.add(AddSpace(2) + 'Result := T' + TabName +
            '.Bitmap(Index, width, height, BitmapScale);');
          DestinationUnit.add('end;');
        end;
        DestinationUnit.add('');
        DestinationUnit.add('initialization');
        DestinationUnit.add('');
        DestinationUnit.add('SetLength(' + TabName + ', ' +
          SVGList.Count.ToString + ');');
        DestinationUnit.add('');
        if UseMultilineString then
          DestinationUnit.add('{$TEXTBLOCK NATIVE XML}');
        for i := 0 to SVGList.Count - 1 do
        begin
          if UseMultilineString then
          begin
            DestinationUnit.add(TabName + '[' + getConstantName(SVGList[i]) +
              '] := ''''''');
            AjouteSVGSource(SVGList[i], DestinationUnit);
            DestinationUnit.add(''''''';');
          end
          else
          begin
            DestinationUnit.add(TabName + '[' + getConstantName(SVGList[i])
              + '] := ');
            AjouteSVGSource(SVGList[i], DestinationUnit, true);
            DestinationUnit.add(';');
          end;
        end;
        DestinationUnit.add('');
        if GenerateOlfSkiaSVGToBitmapCode then
        begin
          DestinationUnit.add('RegisterSVGBitmap;');
          DestinationUnit.add('');
        end;
        DestinationUnit.add('end.');
        DestinationUnit.SaveToFile(ToUnitFilePath);
      finally
        DestinationUnit.Free;
      end;
    end
    else
      raise exception.Create('No SVG file to export !');
  finally
    SVGList.Free;
  end;
end;

end.
