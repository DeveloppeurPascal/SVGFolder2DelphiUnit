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
  File last update : 2025-07-13T10:23:10.000+02:00
  Signature : 171703b79355227b11bd865985aa3efe923d0777
  ***************************************************************************
*)

unit uTxtAboutDescription;

interface

function GetTxtAboutDescription(const Language: string;
  const Recursif: boolean = false): string;

implementation

// For the languages codes, please use 2 letters ISO codes
// https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes

uses
  System.SysUtils,
  uConsts;

const
  CTxtEN = '''
This program is designed for Delphi developers wishing to use vector images (in SVG format) into their projects.

It generates a unit from the list of SVG files contained in a folder.

An SVG list is created as an array of multilines strings containing the textual source code of each vector image.

Constants and an enumeration containing the unit name and the SVG file names provide the indices of the associated source code in the list. This makes it easier to find your way around and manage several SVG lists in a single project.

*****************
* Credits

This application was developed by Patrick Prémartin.

*****************
* Publisher info

This software is published by OLF SOFTWARE, a company registered in Paris (France) under the reference 439521725.

****************
* Personal data

This program is autonomous in its current version. It communicates nothing to the outside world. Your data are private !

We have no knowledge of what you do with it.

No information about you is transmitted to us or to any third party.

We use no cookies, no tracking, no stats on your use of the application.

***************
* User support

If you have any questions or require additional functionality, please leave us a message on the application's website or on its code repository.

To find out more, visit https://svgfolder2delphiunit.olfsoftware.fr

''';
   CTxtFR = '''
Ce programme est destiné à des développeurs Delphi désireux d'intégrer des images vectorielles (sous forme de SVG) dans leurs projets.

Il génère une unité à partir de la liste des fichiers SVG contenus dans un dossier.

Un liste des SVG est créé sous forme de tableau de chaînes de caractères contenant le code source textuel de chaque image vectorielle.

Des constantes et une énumération reprenant le nom de l'unité et les noms des fichiers SVG fournissent les indices du code source associé dans la liste. C'est plus simple pour s'y retrouver et en gérer plusieurs dans un même projet.

*****************
* Remerciements

Cette application a été développée par Patrick Prémartin.

*****************
* Info éditeur

Ce logiciel est éditée par OLF SOFTWARE, société enregistrée à Paris (France) sous la référence 439521725.

****************
* Données personnelles

Ce programme est autonome dans sa version actuelle. Il ne communique rien au monde extérieur. Vos données sont privées !

Nous n'avons aucune connaissance de ce que vous faites avec lui.

Aucune information vous concernant n'est transmise à nous ou à des tiers.

Nous n'utilisons pas de cookies, pas de tracking, pas de statistiques sur votre utilisation de l'application.

***************
* Assistance aux utilisateurs

Si vous avez des questions ou si vous avez besoin de fonctionnalités supplémentaires, veuillez nous laisser un message sur le site web de l'application ou sur son dépôt de code.

Pour en savoir plus, visitez https://svgfolder2delphiunit.olfsoftware.fr

''';
  // CTxtIT = '';
  // CTxtDE = '';
  // CTxtJP = '';
  // CTxtPT = '';
  // CTxtES = '';

function GetTxtAboutDescription(const Language: string;
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
    result := GetTxtAboutDescription(CDefaultLanguage, true)
  else
    raise Exception.Create('Unknow description for language "' +
      Language + '".');
end;

end.
