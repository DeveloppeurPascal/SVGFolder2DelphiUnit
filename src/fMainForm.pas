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
  File last update : 2025-07-13T14:35:54.000+02:00
  Signature : e8f0470373957c41efc304fdef409228a830e08f
  ***************************************************************************
*)

unit fMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  _MainFormAncestor,
  System.Actions,
  FMX.ActnList,
  FMX.Menus,
  uDocumentsAncestor,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Controls.Presentation,
  Olf.FMX.SelectDirectory,
  FMX.Edit;

type
  TMainForm = class(T__MainFormAncestor)
    VertScrollBox1: TVertScrollBox;
    lblFoldersToImport: TLabel;
    lbFoldersToImport: TListBox;
    gplFoldersToImport: TGridPanelLayout;
    btnAddFolder: TButton;
    btnRemoveFolder: TButton;
    btnExport: TButton;
    sdImportFolder: TOlfSelectDirectoryDialog;
    tbHeader: TToolBar;
    btnReset: TButton;
    tbFooter: TToolBar;
    btnAbout: TButton;
    btnClose: TButton;
    sdDestUnit: TSaveDialog;
    lblGeneratedName: TLabel;
    edtGeneratedName: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
    procedure btnAddFolderClick(Sender: TObject);
    procedure lbFoldersToImportDragOver(Sender: TObject;
      const Data: TDragObject; const Point: TPointF;
      var Operation: TDragOperation);
    procedure lbFoldersToImportDragDrop(Sender: TObject;
      const Data: TDragObject; const Point: TPointF);
    procedure btnRemoveFolderClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
  private
  protected
    procedure ResetFields;
    procedure DoDocumentNewAction(Sender: TObject); override;
    procedure AddFolderToList(const Folder: string);
  public
    procedure TranslateTexts(const Language: string); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  uSF2DUExport;

procedure TMainForm.AddFolderToList(const Folder: string);
var
  i: integer;
  ct: integer;
begin
  if Folder.IsEmpty or (not TDirectory.Exists(Folder)) then
    exit;

  for i := 0 to lbFoldersToImport.Items.Count - 1 do
  begin
    ct := comparetext(lbFoldersToImport.Items[i], Folder);
    if ct = 0 then // equal
      exit
    else if ct > 0 then // current folder is greater than the folder to add
      break;
  end;

  lbFoldersToImport.Items.Add(Folder);

  if edtGeneratedName.Text.Trim.IsEmpty then
    edtGeneratedName.Text := 'SVG' +
      OnlyChar(TPath.GetFileNameWithoutExtension(Folder));
end;

procedure TMainForm.btnAboutClick(Sender: TObject);
begin
  DoAboutAction(Sender);
end;

procedure TMainForm.btnAddFolderClick(Sender: TObject);
begin
  if sdImportFolder.Root.IsEmpty then
    sdImportFolder.Root := TPath.GetDocumentsPath;
  if sdImportFolder.Execute then
  begin
    AddFolderToList(sdImportFolder.Directory);
    sdImportFolder.Root := sdImportFolder.Directory;
  end;
end;

procedure TMainForm.btnExportClick(Sender: TObject);
begin
  if lbFoldersToImport.Items.Count < 1 then
    raise exception.Create('At least one folder is needed !');
  // TODO : traduire texte

  edtGeneratedName.Text := OnlyChar(edtGeneratedName.Text.Trim);
  if edtGeneratedName.Text.IsEmpty then
    raise exception.Create
      ('Need a valid Pascal identifier for the unit, classes, enums and constants.');
  // TODO : traduire texte

  sdDestUnit.InitialDir := lbFoldersToImport.Items[0];
  sdDestUnit.FileName := 'u' + edtGeneratedName.Text + '.pas';
  if sdDestUnit.Execute then
  begin
    ExportFoldersToPascalUnit(lbFoldersToImport.Items.ToStringArray,
      sdDestUnit.FileName, edtGeneratedName.Text);
    ShowMessage('Export terminé');
  end;
end;

procedure TMainForm.btnRemoveFolderClick(Sender: TObject);
begin
  if assigned(lbFoldersToImport.Selected) then
    lbFoldersToImport.Selected.Free;
end;

procedure TMainForm.DoDocumentNewAction(Sender: TObject);
begin
  ResetFields;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ResetFields;
end;

procedure TMainForm.lbFoldersToImportDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
var
  i: integer;
begin
  for i := 0 to length(Data.Files) - 1 do
    AddFolderToList(Data.Files[i]);
end;

procedure TMainForm.lbFoldersToImportDragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
var
  i: integer;
begin
  if length(Data.Files) > 0 then
    for i := 0 to length(Data.Files) - 1 do
      if TDirectory.Exists(Data.Files[i]) then
      begin
        Operation := TDragOperation.Copy;
        exit;
      end;
  Operation := TDragOperation.None;
end;

procedure TMainForm.ResetFields;
begin
  lbFoldersToImport.Clear;
  edtGeneratedName.Text := '';
end;

procedure TMainForm.TranslateTexts(const Language: string);
begin
  inherited;
  if Language = 'fr' then
  begin
    lblFoldersToImport.Text := 'Dossiers des SVG';
    btnAddFolder.Text := 'Ajouter';
    btnRemoveFolder.Text := 'Retirer';
    sdImportFolder.Text := 'Choisissez un dossier';
    btnExport.Text := 'Exporter';
    btnAbout.Text := 'A propos';
    lblGeneratedName.Text := 'Préfixe des constantes, classes et types générés';
  end
  else
  begin
    lblFoldersToImport.Text := 'SVG folders';
    btnAddFolder.Text := 'Add';
    btnRemoveFolder.Text := 'Remove';
    sdImportFolder.Text := 'Choose a folder';
    btnExport.Text := 'Export';
    btnAbout.Text := 'About';
    lblGeneratedName.Text :=
      'Prefix for generated constants, classes and types';
  end;
end;

end.
