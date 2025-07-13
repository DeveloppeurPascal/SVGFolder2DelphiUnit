(* C2PP
  ***************************************************************************

  FMX Tools Starter Kit

  Copyright 2024-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  ***************************************************************************

  A starter kit for your FireMonkey projects in Delphi.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://fmxtoolsstarterkit.developpeur-pascal.fr/

  Project site :
  https://github.com/DeveloppeurPascal/FMX-Tools-Starter-Kit

  ***************************************************************************
  File last update : 2025-05-24T20:31:55.364+02:00
  Signature : 13c3771067c1180f047edd7f75b763c71ba1c1da
  ***************************************************************************
*)

unit fMainForm;

interface

{$MESSAGE WARN 'Save this file to your project directory. It''s your main form.'}
// TODO : Save this file to your project directory. It's your main form.

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
  uDocumentsAncestor;

type
  TMainForm = class(T__MainFormAncestor)
  private
  protected
    function GetNewDoc(const FileName: string = ''): TDocumentAncestor;
      override;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{ TMainForm }

function TMainForm.GetNewDoc(const FileName: string): TDocumentAncestor;
begin
{$MESSAGE WARN 'Create an instance of your document and remove this comment.'}
  // TODO : Create an instance of your document and remove this comment
  // result := TYourDocumentType.Create;
  result := nil;
end;

end.
