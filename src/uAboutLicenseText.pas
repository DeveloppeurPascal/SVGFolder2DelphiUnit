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
/// File last update : 2024-08-06T18:27:44.000+02:00
/// Signature : 1a2521828c109486d9f316f8d5e743f79e751ccc
/// ***************************************************************************
/// </summary>

unit uAboutLicenseText;

interface

const
  CAboutLicenseEN = '''
This program is distributed as shareware. If you use it (especially for commercial or income-generating purposes), please remember the author and contribute to its development by purchasing a license.

This software is supplied as is, with or without bugs. No warranty is offered as to its operation or the data processed. Make backups!
''';

implementation

end.
