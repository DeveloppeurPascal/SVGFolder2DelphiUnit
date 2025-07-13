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
  File last update : 2025-07-13T10:31:26.490+02:00
  Signature : 3a75e836bff54c6cd233e0c0a0629665f12a1180
  ***************************************************************************
*)

unit uTxtAboutLicense;

interface

function GetTxtAboutLicense(const Language: string;
  const Recursif: boolean = false): string;

implementation

// For the languages codes, please use 2 letters ISO codes
// https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes

uses
  System.SysUtils,
  uConsts;

const
  CTxtEN = '''
This program is distributed as shareware. If you use it (especially for commercial or income-generating purposes), please remember the author and contribute to its development by purchasing a license.

This software is supplied as is, with or without bugs. No warranty is offered as to its operation or the data processed. Make backups!

''';
  CTxtFR = '''
Ce programme est distribué en tant que shareware. Si vous l'utilisez (en particulier à des fins commerciales ou rémunératrices), merci de vous souvenir de l'auteur et de contribuer à son développement en achetant une licence.

Ce logiciel est fourni tel quel, avec ou sans bogues. Aucune garantie n'est offerte quant à son fonctionnement ou aux données traitées. Faites des sauvegardes !

''';
  // CTxtIT = '';
  // CTxtDE = '';
  // CTxtJP = '';
  // CTxtPT = '';
  // CTxtES = '';

function GetTxtAboutLicense(const Language: string;
  const Recursif: boolean): string;
var
  lng: string;
begin
  lng := Language.tolower;
  if (lng = 'en') then
    result := CTxtEN
  else if (lng = 'fr') then // France
    result := CTxtFR
  else if not Recursif then
    result := GetTxtAboutLicense(CDefaultLanguage, true)
  else
    raise Exception.Create('Unknow license for language "' + Language + '".');
end;

end.
