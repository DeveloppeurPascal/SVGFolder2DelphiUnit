unit uAboutBox;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  Olf.FMX.AboutDialog,
  Olf.FMX.AboutDialogForm,
  System.ImageList,
  FMX.ImgList;

type
  TAboutBox = class(TDataModule)
    ImageList1: TImageList;
    OlfAboutDialog1: TOlfAboutDialog;
    procedure OlfAboutDialog1URLClick(const AURL: string);
    function OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
      const ATxtID: TOlfAboutDialogTxtID): string;
    procedure DataModuleCreate(Sender: TObject);
  private
  public
    class procedure Execute(const Show: boolean = true);
    class function Current: TAboutBox;
  end;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  u_urlOpen,
  uAboutDescriptionText,
  uAboutLicenseText,
  uConsts;

{$R *.dfm}

var
  AboutBox: TAboutBox;

class function TAboutBox.Current: TAboutBox;
begin
  if not assigned(AboutBox) then
    AboutBox := TAboutBox.Create(nil);

  result := AboutBox;
end;

procedure TAboutBox.DataModuleCreate(Sender: TObject);
begin
  OlfAboutDialog1.VersionNumero := CVersionNumber;
  OlfAboutDialog1.VersionDate := CVersionDate;
end;

class procedure TAboutBox.Execute(const Show: boolean);
begin
  if not assigned(AboutBox) then
    AboutBox := TAboutBox.Create(nil);

  if Show then
    AboutBox.OlfAboutDialog1.Execute;
end;

function TAboutBox.OlfAboutDialog1GetText(const ALang: TOlfAboutDialogLang;
  const ATxtID: TOlfAboutDialogTxtID): string;
begin // TODO : traduire les textes
  if ATxtID = TOlfAboutDialogTxtID.LicenseText then
    result := CAboutLicenseEN
  else if ATxtID = TOlfAboutDialogTxtID.DescriptionText then
    result := CAboutDescriptionEN;
end;

procedure TAboutBox.OlfAboutDialog1URLClick(const AURL: string);
begin
  url_Open_In_Browser(AURL);
end;

initialization

AboutBox := nil;
tthread.ForceQueue(nil,
  procedure
  begin
    TAboutBox.Execute(false);
  end);

finalization

AboutBox.free;

end.
