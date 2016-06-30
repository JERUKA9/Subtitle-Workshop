{*
 *  URUWorks Subtitle Workshop
 *
 *  Author  : URUWorks
 *  Website : uruworks.net
 *
 *  License : GNU/GPL v3
 *
 *  Software distributed under the License is distributed on an
 *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 *  implied. See the License for the specific language governing
 *  rights and limitations under the License.
 *
 *  Copyright (C) 2001-2016 URUWorks.
 *}

unit UTexts;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, UWControls;

type

  { TfrmTexts }

  TfrmTexts = class(TForm)
    btnApply: TButton;
    btnClose: TButton;
    chkLowercase: TRadioButton;
    chkUppercase: TRadioButton;
    chkTitleType: TRadioButton;
    chkInverseType: TRadioButton;
    lblApplyIn: TLabel;
    lyoOptions: TUWLayout;
    pagTexts: TPageControl;
    chkSentenceType: TRadioButton;
    rdoAllTheSubtitles: TRadioButton;
    rdoFromTheSelectedSubtitle: TRadioButton;
    rdoOnlyMarkedSubtitles: TRadioButton;
    rdoOnlySelectedSubtitles: TRadioButton;
    tabConvertCase: TTabSheet;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pagTextsChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTexts: TfrmTexts;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain, UWSubtitleAPI, UWSystem.StrUtils, jsonConf;

{$R *.lfm}

// -----------------------------------------------------------------------------

procedure ApplyTexts(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with frmTexts, Item^ do
  begin
    if chkSentenceType.Checked then
    begin
      SetSubtitleText(Index, SentenceCase(Text), True, False, False);
      SetSubtitleText(Index, SentenceCase(Translation), False, False, False);
    end
    else if chkLowercase.Checked then
    begin
      SetSubtitleText(Index, LowerCase(Text), True, False, False);
      SetSubtitleText(Index, LowerCase(Translation), False, False, False);
    end
    else if chkUppercase.Checked then
    begin
      SetSubtitleText(Index, UpperCase(Text), True, False, False);
      SetSubtitleText(Index, UpperCase(Translation), False, False, False);
    end
    else if chkTitleType.Checked then
      begin
        SetSubtitleText(Index, TitleCase(Text), True, False, False);
        SetSubtitleText(Index, TitleCase(Translation), False, False, False);
      end
    else
    begin
      SetSubtitleText(Index, InvertCase(Text), True, False, False);
      SetSubtitleText(Index, InvertCase(Translation), False, False, False);
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ TfrmTexts }

// -----------------------------------------------------------------------------

procedure TfrmTexts.FormCreate(Sender: TObject);
begin
  if frmMain.VST.SelectedCount = 1 then
    rdoFromTheSelectedSubtitle.Checked := True
  else if frmMain.VST.SelectedCount > 1 then
    rdoOnlySelectedSubtitles.Checked := True
  else
    rdoAllTheSubtitles.Checked := True;

  ReadLangForForm(LanguageFileName, Self);

  with TJSONConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    OpenKey('Texts', True);

    chkSentenceType.Checked := GetValue('chkSentenceType', True);
    chkLowercase.Checked    := GetValue('chkLowercase', False);
    chkUppercase.Checked    := GetValue('chkUppercase', False);
    chkTitleType.Checked    := GetValue('chkTitleType', False);
    chkInverseType.Checked  := GetValue('chkInverseType', False);

    CloseKey;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with TJSONConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    OpenKey('Texts', True);

    SetValue('chkSentenceType', chkSentenceType.Checked);
    SetValue('chkLowercase', chkLowercase.Checked);
    SetValue('chkUppercase', chkUppercase.Checked);
    SetValue('chkTitleType', chkTitleType.Checked);
    SetValue('chkInverseType', chkInverseType.Checked);

    CloseKey;
    Flush;
  finally
    Free;
  end;

  CloseAction := caFree;
  frmTexts := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.pagTextsChange(Sender: TObject);
begin
  lyoOptions.Parent := pagTexts.Pages[pagTexts.PageIndex];
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTexts.btnApplyClick(Sender: TObject);
var
  Selection: TUWSubtitleDoLoopSelection;
begin
  if rdoAllTheSubtitles.Checked then
    Selection := dlAll
  else if rdoOnlySelectedSubtitles.Checked then
    Selection := dlSelected
  else if rdoFromTheSelectedSubtitle.Checked then
    Selection := dlCurrentToLast
  else
    Selection := dlMarked;

  if pagTexts.PageIndex = 0 then
    VSTDoLoop(@ApplyTexts, Selection);

  Close;
end;

// -----------------------------------------------------------------------------

end.

