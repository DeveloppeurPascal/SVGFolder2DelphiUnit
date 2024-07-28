unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
  private
  public
    function OnlyChar(const S: string): string;
    function getConstantName(Const FileName: string): string;
    procedure AjouteSVGSource(Const FileName: string;
      var Destination: TStringList);
    function AddSpace(const Nb: cardinal): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

function TForm1.AddSpace(const Nb: cardinal): string;
var
  i: cardinal;
begin
  result := '';
  for i := 1 to Nb do
    result := result + ' ';
end;

procedure TForm1.AjouteSVGSource(const FileName: string;
  var Destination: TStringList);
const
  CMaxLineLength = 1000;
var
  SVGSource: TStringDynArray;
  i: integer;
  CharPos: integer;
begin
  SVGSource := tfile.ReadAllLines(FileName);
  for i := 0 to length(SVGSource) - 1 do
  begin
    // replace a SPACE by a NEW LINE
    if (SVGSource[i].length > CMaxLineLength) then
    begin
      SVGSource[i] := SVGSource[i].trim;
      CharPos := SVGSource[i].LastIndexOf(' ', CMaxLineLength);
      while (CharPos > -1) and (SVGSource[i].length > CMaxLineLength) do
      begin
        Destination.add(SVGSource[i].Substring(0, CharPos + 1).trim);
        SVGSource[i] := SVGSource[i].Substring(CharPos + 1).trim;
        CharPos := SVGSource[i].LastIndexOf(' ', CMaxLineLength);
      end;
    end;
    // replace a COMMA by a NEW LINE
    if (SVGSource[i].length > CMaxLineLength) then
    begin
      SVGSource[i] := SVGSource[i].trim;
      CharPos := SVGSource[i].LastIndexOf(',', CMaxLineLength);
      while (CharPos > -1) and (SVGSource[i].length > CMaxLineLength) do
      begin
        Destination.add(SVGSource[i].Substring(0, CharPos + 1).trim);
        SVGSource[i] := SVGSource[i].Substring(CharPos + 1).trim;
        CharPos := SVGSource[i].LastIndexOf(',', CMaxLineLength);
      end;
    end;
    // add the end of the line
    Destination.add(SVGSource[i]);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  SourceFolder, SourceFolderName: string;
  Files: TStringDynArray;
  SVGFilesList: TStringList;
  i: integer;
  DestinationUnit: TStringList;
  UnitFilePath, UnitFileName: string;
  ArrayName: string;
begin
  if SelectDirectory('SVG source folder', '', SourceFolder) and
    tdirectory.exists(SourceFolder) then
  begin
    Files := tdirectory.GetFiles(SourceFolder);
    if (length(Files) > 0) then
    begin
      SaveDialog1.InitialDir := SourceFolder;
      SourceFolderName := OnlyChar(tpath.GetFileName(SourceFolder));
      SaveDialog1.FileName := tpath.combine(SourceFolder,
        'uSVG' + SourceFolderName + '.pas');
      if SaveDialog1.Execute then
      begin
        UnitFileName :=
          OnlyChar(tpath.GetFileNameWithoutExtension(SaveDialog1.FileName));
        UnitFilePath := tpath.combine
          (tpath.GetDirectoryName(SaveDialog1.FileName), UnitFileName + '.pas');

        SVGFilesList := TStringList.Create;
        try
          for i := 0 to length(Files) - 1 do
            if Files[i].ToLower.EndsWith('.svg') then
              SVGFilesList.add(Files[i]);

          if (SVGFilesList.Count > 0) then
          begin
            SVGFilesList.Sort;
            DestinationUnit := TStringList.Create;
            try
              DestinationUnit.add('unit ' + UnitFileName + ';');
              DestinationUnit.add('');
              DestinationUnit.add
                ('// ****************************************');
              DestinationUnit.add('// * SVG from folder :');
              DestinationUnit.add('// * ' + SaveDialog1.FileName);
              DestinationUnit.add
                ('// ****************************************');
              DestinationUnit.add('//');
              DestinationUnit.add
                ('// This file contains a list of contants and ');
              DestinationUnit.add
                ('// an enumeration to access to SVG source codes ');
              DestinationUnit.add('// from the generated array of strings.');
              DestinationUnit.add('//');
              DestinationUnit.add
                ('// ****************************************');
              // TODO : remplacer le nom du projet par le titre de la boite de dialogue "à propos"
              // TODO : remplacer la version du projet par la version de la boite de dialogue "à propos"
              DestinationUnit.add
                ('// File generator : SVG Folder to Delphi Unit (1.0)');
              // TODO : remplacer l'URL par l'URL de la boite de dialogue "à propos"
              DestinationUnit.add
                ('// Website : https://svgfolder2delphiunit.olfsoftware.fr/');
              DestinationUnit.add('// Generation date : ' + DateTimeToStr(now));
              DestinationUnit.add('//');
              DestinationUnit.add('// Don''t do any change on this file.');
              DestinationUnit.add
                ('// They will be erased by next generation !');
              DestinationUnit.add
                ('// ****************************************');
              DestinationUnit.add('');
              DestinationUnit.add('interface');
              DestinationUnit.add('');
              DestinationUnit.add('const');
              for i := 0 to SVGFilesList.Count - 1 do
                DestinationUnit.add
                  (AddSpace(2) + getConstantName(SVGFilesList[i]) + ' = ' +
                  i.tostring + ';');
              ArrayName := 'SVG' + SourceFolderName;
              DestinationUnit.add('');
              DestinationUnit.add('type');
              DestinationUnit.add('{$SCOPEDENUMS ON}');
              DestinationUnit.add(AddSpace(2) + 'T' + ArrayName + 'Index = (');
              for i := 0 to SVGFilesList.Count - 1 do
                if (i < SVGFilesList.Count - 1) then
                  DestinationUnit.add
                    (AddSpace(4) + OnlyChar(tpath.GetFileNameWithoutExtension
                    (SVGFilesList[i])) + ' = ' +
                    getConstantName(SVGFilesList[i]) + ',')
                else
                  DestinationUnit.add
                    (AddSpace(4) + OnlyChar(tpath.GetFileNameWithoutExtension
                    (SVGFilesList[i])) + ' = ' +
                    getConstantName(SVGFilesList[i]) + ');');
              DestinationUnit.add('');
              DestinationUnit.add(AddSpace(2) + 'T' + ArrayName + ' = class');
              DestinationUnit.add(AddSpace(2) + 'private');
              DestinationUnit.add(AddSpace(2) + 'class var');
              DestinationUnit.add(AddSpace(4) + 'FTag: integer;');
              DestinationUnit.add(AddSpace(4) + 'FTagBool: Boolean;');
              DestinationUnit.add(AddSpace(4) + 'FTagFloat: Single;');
              DestinationUnit.add(AddSpace(4) + 'FTagObject: TObject;');
              DestinationUnit.add(AddSpace(4) + 'FTagString: string;');
              DestinationUnit.add
                (AddSpace(4) +
                'class procedure SetTag(const Value: integer); static;');
              DestinationUnit.add
                (AddSpace(4) +
                'class procedure SetTagBool(const Value: Boolean); static;');
              DestinationUnit.add
                (AddSpace(4) +
                'class procedure SetTagFloat(const Value: Single); static;');
              DestinationUnit.add
                (AddSpace(4) +
                'class procedure SetTagObject(const Value: TObject); static;');
              DestinationUnit.add
                (AddSpace(4) +
                'class procedure SetTagString(const Value: string); static;');
              DestinationUnit.add(AddSpace(2) + 'public const');
              for i := 0 to SVGFilesList.Count - 1 do
                DestinationUnit.add
                  (AddSpace(4) + OnlyChar(tpath.GetFileNameWithoutExtension
                  (SVGFilesList[i])) + ' = ' +
                  getConstantName(SVGFilesList[i]) + ';');
              DestinationUnit.add
                (AddSpace(4) +
                'class property Tag: integer read FTag write SetTag;');
              DestinationUnit.add
                (AddSpace(4) +
                'class property TagBool: Boolean read FTagBool write SetTagBool;');
              DestinationUnit.add
                (AddSpace(4) +
                'class property TagFloat: Single read FTagFloat write SetTagFloat;');
              DestinationUnit.add
                (AddSpace(4) +
                'class property TagObject: TObject read FTagObject write SetTagObject;');
              DestinationUnit.add
                (AddSpace(4) +
                'class property TagString: string read FTagString write SetTagString;');
              DestinationUnit.add
                (AddSpace(4) +
                'class function SVG(const Index: Integer): string; overload;');
              DestinationUnit.add
                (AddSpace(4) + 'class function SVG(const Index: T' + ArrayName +
                'Index) : string; overload;');
              DestinationUnit.add
                (AddSpace(4) + 'class function Count : Integer;');
              DestinationUnit.add(AddSpace(4) + 'class constructor Create;');
              DestinationUnit.add(AddSpace(2) + 'end;');
              DestinationUnit.add('');
              DestinationUnit.add('var');
              DestinationUnit.add(AddSpace(2) + ArrayName +
                ' : array of String;');
              DestinationUnit.add('');
              DestinationUnit.add('implementation');
              DestinationUnit.add('');
              DestinationUnit.add('uses');
              DestinationUnit.add(AddSpace(2) + 'System.SysUtils;');
              DestinationUnit.add('');
              DestinationUnit.add('{ T' + ArrayName + ' }');
              DestinationUnit.add('');
              DestinationUnit.add('class constructor T' + ArrayName +
                '.Create;');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'inherited;');
              DestinationUnit.add(AddSpace(2) + 'FTag := 0;');
              DestinationUnit.add(AddSpace(2) + 'FTagBool := false;');
              DestinationUnit.add(AddSpace(2) + 'FTagFloat := 0;');
              DestinationUnit.add(AddSpace(2) + 'FTagObject := nil;');
              DestinationUnit.add(AddSpace(2) + 'FTagString := '''';');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class procedure T' + ArrayName +
                '.SetTag(const Value: integer);');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'FTag := Value;');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class procedure T' + ArrayName +
                '.SetTagBool(const Value: Boolean);');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'FTagBool := Value;');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class procedure T' + ArrayName +
                '.SetTagFloat(const Value: Single);');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'FTagFloat := Value;');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class procedure T' + ArrayName +
                '.SetTagObject(const Value: TObject);');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'FTagObject := Value;');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class procedure T' + ArrayName +
                '.SetTagString(const Value: string);');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'FTagString := Value;');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class function T' + ArrayName +
                '.SVG(const Index: Integer): string;');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'if (index < Count) then');
              DestinationUnit.add(AddSpace(2) + '  result := ' + ArrayName +
                '[index]');
              DestinationUnit.add(AddSpace(2) + 'else');
              DestinationUnit.add
                (AddSpace(2) +
                '  raise Exception.Create(''SVG not found. Index out of range.'');');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class function T' + ArrayName +
                '.SVG(const Index : T' + ArrayName + 'Index): string;');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'result := SVG(ord(index));');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('class function T' + ArrayName +
                '.Count: Integer;');
              DestinationUnit.add('begin');
              DestinationUnit.add(AddSpace(2) + 'result := length(' +
                ArrayName + ');');
              DestinationUnit.add('end;');
              DestinationUnit.add('');
              DestinationUnit.add('initialization');
              DestinationUnit.add('');
              DestinationUnit.add('SetLength(' + ArrayName + ', ' +
                SVGFilesList.Count.tostring + ');');
              DestinationUnit.add('');
              DestinationUnit.add('{$TEXTBLOCK NATIVE XML}');
              for i := 0 to SVGFilesList.Count - 1 do
              begin
                DestinationUnit.add(ArrayName + '[' +
                  getConstantName(SVGFilesList[i]) + '] := ''''''');
                AjouteSVGSource(SVGFilesList[i], DestinationUnit);
                DestinationUnit.add(''''''';');
              end;
              DestinationUnit.add('');
              DestinationUnit.add('end.');
              DestinationUnit.SaveToFile(UnitFilePath);
            finally
              DestinationUnit.Free;
            end;
          end
          else
            raise exception.Create('No SVG in "' + SourceFolder + '" !');
        finally
          SVGFilesList.Free;
        end;
        ShowMessage('Export réussi vers "' + UnitFileName + '".');
      end
      else
        raise exception.Create('Destination unit is needed !');
    end
    else
      raise exception.Create('No file in "' + SourceFolder + '" !');
  end
  else
    raise exception.Create('Source directory needed !');
end;

function TForm1.getConstantName(const FileName: string): string;
begin
  result := 'CSVG' + OnlyChar(tpath.GetFileNameWithoutExtension(FileName));
end;

function TForm1.OnlyChar(const S: string): string;
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
    if CharInSet(c, ['-', '_', '.']) then
      Upper := true
    else if CharInSet(c, ['0' .. '9', 'a' .. 'z', 'A' .. 'Z']) then
      if Upper then
      begin
        Upper := false;
        result := result + UpperCase(c);
      end
      else
        result := result + c;
  end;
end;

end.
