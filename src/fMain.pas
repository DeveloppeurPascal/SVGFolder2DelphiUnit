/// <summary>
/// ***************************************************************************
///
/// SVG folder to Delphi unit
///
/// Copyright 2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// This program is designed for Delphi developers wishing to use vector
/// images (in SVG format) into their projects.
///
/// It generates a unit from the list of SVG files contained in a folder.
///
/// An SVG list is created as an array of multilines strings containing the
/// textual source code of each vector image.
///
/// Constants and an enumeration containing the unit name and the SVG file
/// names provide the indices of the associated source code in the list. This
/// makes it easier to find your way around and manage several SVG lists in a
/// single project.
///
/// You can use the result in a VCL or FireMonkey Delphi project, with Skia
/// enabled (via Skia4Delphi project or natively) and the unit
/// Olf.Skia.SVGToBitmap.pas from
/// https://github.com/DeveloppeurPascal/librairies/
///
/// ***************************************************************************
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://svgfolder2delphiunit.olfsoftware.fr/
///
/// Project site :
/// https://github.com/DeveloppeurPascal/SVGFolder2DelphiUnit
///
/// ***************************************************************************
/// File last update : 2024-07-28T14:24:30.000+02:00
/// Signature : c5a5299fe9ef8942c772a3cc5451bfcd28f992a7
/// ***************************************************************************
/// </summary>

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
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo;

type
  TfrmMain = class(TForm)
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    btnAbout: TButton;
    btnQuit: TButton;
    btnStart: TButton;
    Memo1: TMemo;
    procedure btnAboutClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure StartProcess;
  public
    function OnlyChar(const S: string): string;
    function getConstantName(Const FileName: string): string;
    procedure AjouteSVGSource(Const FileName: string;
      var Destination: TStringList);
    function AddSpace(const Nb: cardinal): string;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.DateUtils,
  System.IOUtils,
  uAboutBox;

function TfrmMain.AddSpace(const Nb: cardinal): string;
var
  i: cardinal;
begin
  result := '';
  for i := 1 to Nb do
    result := result + ' ';
end;

procedure TfrmMain.AjouteSVGSource(const FileName: string;
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

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  TAboutBox.Execute;
end;

procedure TfrmMain.btnQuitClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  StartProcess;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
  // TODO : traduire textes
  Memo1.Lines.add('Click on "Start" button to process.');
  Memo1.Lines.add('Step 1 : choose the folder containing your SVG files');
  Memo1.Lines.add('Step 2 : choose the folder and name of the generated unit');
  Memo1.Lines.add('Step 3 : Enjoy using SVG files in your code !');
end;

function TfrmMain.getConstantName(const FileName: string): string;
begin
  result := 'CSVG' + OnlyChar(tpath.GetFileNameWithoutExtension(FileName));
end;

function TfrmMain.OnlyChar(const S: string): string;
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

procedure TfrmMain.StartProcess;
var
  SourceFolder, SourceFolderName: string;
  Files: TStringDynArray;
  SVGFilesList: TStringList;
  i: integer;
  DestinationUnit: TStringList;
  UnitFilePath, UnitFileName: string;
  ArrayName: string;
begin
  Memo1.Lines.add('----------------------------------------');
  try
    if SelectDirectory('SVG source folder', '', SourceFolder) and
      tdirectory.exists(SourceFolder) then
    begin
      Memo1.Lines.add('SVG folder : ' + SourceFolder);
      Files := tdirectory.GetFiles(SourceFolder);
      if (length(Files) > 0) then
      begin
        SaveDialog1.InitialDir := SourceFolder;
        SourceFolderName := OnlyChar(tpath.GetFileName(SourceFolder));
        // SaveDialog1.FileName := tpath.combine(SourceFolder,
        // 'uSVG' + SourceFolderName + '.pas');
        SaveDialog1.FileName := 'uSVG' + SourceFolderName + '.pas';
        if SaveDialog1.Execute then
        begin
          UnitFileName :=
            OnlyChar(tpath.GetFileNameWithoutExtension(SaveDialog1.FileName));
          UnitFilePath :=
            tpath.combine(tpath.GetDirectoryName(SaveDialog1.FileName),
            UnitFileName + '.pas');

          Memo1.Lines.add('Destination unit : ' + UnitFilePath);

          SVGFilesList := TStringList.Create;
          try
            for i := 0 to length(Files) - 1 do
              if Files[i].ToLower.EndsWith('.svg') then
                SVGFilesList.add(Files[i]);

            Memo1.Lines.add('SVG files found : ' + SVGFilesList.Count.ToString);

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
                DestinationUnit.add('// File generator : ' +
                  TAboutBox.Current.OlfAboutDialog1.GetMainFormCaption);
                DestinationUnit.add('// Website : ' +
                  TAboutBox.Current.OlfAboutDialog1.URL);
                DestinationUnit.add('// Generation date : ' +
                  DateToISO8601(now, true));
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
                    i.ToString + ';');
                ArrayName := 'SVG' + SourceFolderName;
                DestinationUnit.add('');
                DestinationUnit.add('type');
                DestinationUnit.add('{$SCOPEDENUMS ON}');
                DestinationUnit.add(AddSpace(2) + 'T' + ArrayName +
                  'Index = (');
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
                  (AddSpace(4) + 'class function SVG(const Index: T' + ArrayName
                  + 'Index) : string; overload;');
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
                  SVGFilesList.Count.ToString + ');');
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
          ShowMessage('Unit "' + tpath.GetFileName(UnitFilePath) +
            '" generated.');
        end
        else
          raise exception.Create('Destination unit is needed !');
      end
      else
        raise exception.Create('No file in "' + SourceFolder + '" !');
    end
    else
      raise exception.Create('Source directory needed !');
  except
    on E: exception do
    begin
      if E.ClassName.ToLower.Equals('exception') then
        Memo1.Lines.add('***** Error *****')
      else
        Memo1.Lines.add('***** Error ' + E.ClassName + ' *****');
      Memo1.Lines.add(E.Message);
    end;
  end;
  Memo1.GoToTextEnd;
end;

end.
