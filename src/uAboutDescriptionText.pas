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
/// File last update : 2024-08-06T18:29:16.000+02:00
/// Signature : 2c93cace817ea0837f026e26c8eaf82d75b9ae38
/// ***************************************************************************
/// </summary>

unit uAboutDescriptionText;

interface

const
  CAboutDescriptionEN = '''
This program is designed for Delphi developers wishing to use vector images (in SVG format) into their projects.

It generates a unit from the list of SVG files contained in a folder.

An SVG list is created as an array of multilines strings containing the textual source code of each vector image.

Constants and an enumeration containing the unit name and the SVG file names provide the indices of the associated source code in the list. This makes it easier to find your way around and manage several SVG lists in a single project.

*****************
* Publisher info

This application was developed by Patrick Prémartin in Delphi.

It is published by OLF SOFTWARE, a company registered in Paris (France) under the reference 439521725.

****************
* Personal data

This program is autonomous in its current version. It does not depend on the Internet and communicates nothing to the outside world.

We have no knowledge of what you do with it.

No information about you is transmitted to us or to any third party.

We use no cookies, no tracking, no stats on your use of the application.

***************
* User support

If you have any questions or require additional functionality, please leave us a message on the application''s website or on its code repository.

To find out more, visit https://svgfolder2delphiunit.olfsoftware.fr/
''';

implementation

end.
