unit USVGSampleSVGFromGoogle;

// ****************************************
// * SVG from folder :
// * C:\Users\patrickpremartin\Documents\Embarcadero\Studio\Projets\SVGFolder2DelphiUnit\assets\SampleSVGFromGoogle\USVGSampleSVGFromGoogle.pas
// ****************************************
//
// This file contains a list of contants and 
// an enumeration to access to SVG source codes 
// from the generated array of strings.
//
// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 28/07/2024 14:24:54
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

interface

const
  CSVGAccount = 0;
  CSVGAdjust = 1;
  CSVGArrowBottomRightThinCircleOutline = 2;
  CSVGBabyFaceOutline = 3;
  CSVGBike = 4;
  CSVGBookmark = 5;
  CSVGBroadcast = 6;

type
{$SCOPEDENUMS ON}
  TSVGSampleSVGFromGoogleIndex = (
    Account = CSVGAccount,
    Adjust = CSVGAdjust,
    ArrowBottomRightThinCircleOutline = CSVGArrowBottomRightThinCircleOutline,
    BabyFaceOutline = CSVGBabyFaceOutline,
    Bike = CSVGBike,
    Bookmark = CSVGBookmark,
    Broadcast = CSVGBroadcast);

  TSVGSampleSVGFromGoogle = class
  private
  class var
    FTag: integer;
    FTagBool: Boolean;
    FTagFloat: Single;
    FTagObject: TObject;
    FTagString: string;
    class procedure SetTag(const Value: integer); static;
    class procedure SetTagBool(const Value: Boolean); static;
    class procedure SetTagFloat(const Value: Single); static;
    class procedure SetTagObject(const Value: TObject); static;
    class procedure SetTagString(const Value: string); static;
  public const
    Account = CSVGAccount;
    Adjust = CSVGAdjust;
    ArrowBottomRightThinCircleOutline = CSVGArrowBottomRightThinCircleOutline;
    BabyFaceOutline = CSVGBabyFaceOutline;
    Bike = CSVGBike;
    Bookmark = CSVGBookmark;
    Broadcast = CSVGBroadcast;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: Boolean read FTagBool write SetTagBool;
    class property TagFloat: Single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: Integer): string; overload;
    class function SVG(const Index: TSVGSampleSVGFromGoogleIndex) : string; overload;
    class function Count : Integer;
    class constructor Create;
  end;

var
  SVGSampleSVGFromGoogle : array of String;

implementation

uses
  System.SysUtils;

{ TSVGSampleSVGFromGoogle }

class constructor TSVGSampleSVGFromGoogle.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGSampleSVGFromGoogle.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGSampleSVGFromGoogle.SetTagBool(const Value: Boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGSampleSVGFromGoogle.SetTagFloat(const Value: Single);
begin
  FTagFloat := Value;
end;

class procedure TSVGSampleSVGFromGoogle.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGSampleSVGFromGoogle.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGSampleSVGFromGoogle.SVG(const Index: Integer): string;
begin
  if (index < Count) then
    result := SVGSampleSVGFromGoogle[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGSampleSVGFromGoogle.SVG(const Index : TSVGSampleSVGFromGoogleIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGSampleSVGFromGoogle.Count: Integer;
begin
  result := length(SVGSampleSVGFromGoogle);
end;

initialization

SetLength(SVGSampleSVGFromGoogle, 7);

{$TEXTBLOCK NATIVE XML}
SVGSampleSVGFromGoogle[CSVGAccount] := '''
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M12,4A4,4 0 0,1 16,8A4,4 0 0,1 12,12A4,4 0 0,1 8,8A4,4 0 0,1 12,4M12,14C16.42,14 20,15.79 20,18V20H4V18C4,15.79 7.58,14 12,14Z" /></svg>
''';
SVGSampleSVGFromGoogle[CSVGAdjust] := '''
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M15,12A3,3 0 0,1 12,15A3,3 0 0,1 9,12A3,3 0 0,1 12,9A3,3 0 0,1 15,12Z" /></svg>
''';
SVGSampleSVGFromGoogle[CSVGArrowBottomRightThinCircleOutline] := '''
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M12 20.03C7.59 20.03 3.97 16.41 3.97 12C3.97 7.59 7.59 3.97 12 3.97C16.41 3.97 20.03 7.59 20.03 12C20.03 16.41 16.41 20.03 12 20.03M12 22C17.54 22 22 17.54 22 12C22 6.46 17.54 2 12 2C6.46 2 2 6.46 2 12C2 17.54 6.46 22 12 22M13.88 12.47L16 10.36V16H10.36L12.47 13.88L7.5 8.9L8.9 7.5" /></svg>
''';
SVGSampleSVGFromGoogle[CSVGBabyFaceOutline] := '''
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M14.5,9.25A1.25,1.25 0 0,1 15.75,10.5A1.25,1.25 0 0,1 14.5,11.75A1.25,1.25 0 0,1 13.25,10.5A1.25,1.25 0 0,1 14.5,9.25M9.5,9.25A1.25,1.25 0 0,1 10.75,10.5A1.25,1.25 0 0,1 9.5,11.75A1.25,1.25 0 0,1 8.25,10.5A1.25,1.25 0 0,1 9.5,9.25M7.5,14H16.5C15.74,15.77 14,17 12,17C10,17 8.26,15.77 7.5,14M1,12C1,10.19 2.2,8.66 3.86,8.17C5.29,5.11 8.4,3 12,3C15.6,3 18.71,5.11 20.15,8.17C21.8,8.66 23,10.19 23,12C23,13.81 21.8,15.34 20.15,15.83C18.71,18.89 15.6,21 12,21C8.4,21 5.29,18.89 3.86,15.83C2.2,15.34 1,13.81 1,12M12,5C8.82,5 6.14,7.12 5.28,10H5A2,2 0 0,0 3,12A2,2 0 0,0 5,14H5.28C6.14,16.88 8.82,19 12,19C15.18,19 17.86,16.88 18.72,14H19A2,2 0 0,0 21,12A2,2 0 0,0 19,10H18.72C17.86,7.12 15.18,5 12,5Z" /></svg>
''';
SVGSampleSVGFromGoogle[CSVGBike] := '''
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M5,20.5A3.5,3.5 0 0,1 1.5,17A3.5,3.5 0 0,1 5,13.5A3.5,3.5 0 0,1 8.5,17A3.5,3.5 0 0,1 5,20.5M5,12A5,5 0 0,0 0,17A5,5 0 0,0 5,22A5,5 0 0,0 10,17A5,5 0 0,0 5,12M14.8,10H19V8.2H15.8L13.86,4.93C13.57,4.43 13,4.1 12.4,4.1C11.93,4.1 11.5,4.29 11.2,4.6L7.5,8.29C7.19,8.6 7,9 7,9.5C7,10.13 7.33,10.66 7.85,10.97L11.2,13V18H13V11.5L10.75,9.85L13.07,7.5M19,20.5A3.5,3.5 0 0,1 15.5,17A3.5,3.5 0 0,1 19,13.5A3.5,3.5 0 0,1 22.5,17A3.5,3.5 0 0,1 19,20.5M19,12A5,5 0 0,0 14,17A5,5 0 0,0 19,22A5,5 0 0,0 24,17A5,5 0 0,0 19,12M16,4.8C17,4.8 17.8,4 17.8,3C17.8,2 17,1.2 16,1.2C15,1.2 14.2,2 14.2,3C14.2,4 15,4.8 16,4.8Z" /></svg>
''';
SVGSampleSVGFromGoogle[CSVGBookmark] := '''
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M17,3H7A2,2 0 0,0 5,5V21L12,18L19,21V5C19,3.89 18.1,3 17,3Z" /></svg>
''';
SVGSampleSVGFromGoogle[CSVGBroadcast] := '''
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><path d="M12 10C10.9 10 10 10.9 10 12S10.9 14 12 14 14 13.1 14 12 13.1 10 12 10M18 12C18 8.7 15.3 6 12 6S6 8.7 6 12C6 14.2 7.2 16.1 9 17.2L10 15.5C8.8 14.8 8 13.5 8 12.1C8 9.9 9.8 8.1 12 8.1S16 9.9 16 12.1C16 13.6 15.2 14.9 14 15.5L15 17.2C16.8 16.2 18 14.2 18 12M12 2C6.5 2 2 6.5 2 12C2 15.7 4 18.9 7 20.6L8 18.9C5.6 17.5 4 14.9 4 12C4 7.6 7.6 4 12 4S20 7.6 20 12C20 15 18.4 17.5 16 18.9L17 20.6C20 18.9 22 15.7 22 12C22 6.5 17.5 2 12 2Z" /></svg>
''';

end.
