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

unit UTimings;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, UWControls;

type

  { TfrmTimings }

  TfrmTimings = class(TForm)
    btnClose: TButton;
    btnApply: TButton;
    cboDelay: TComboBox;
    chkOnlyIfDurationLongerThan: TCheckBox;
    chkSetMaximumDuration: TCheckBox;
    chkOnlyIfSubtitleLongerThan: TCheckBox;
    chkSetMinimumDuration: TCheckBox;
    cboCondition: TComboBox;
    cboExpanderReducer: TComboBox;
    lblExpandMilliseconds: TLabel;
    lblPerCharacter: TLabel;
    lblCondition: TLabel;
    lblApplyIn: TLabel;
    lblLongerCharacters: TLabel;
    lblLongerMilliseconds: TLabel;
    lblPerWord: TLabel;
    lblPerLine: TLabel;
    lblPerSecond: TLabel;
    lyoOptions: TUWLayout;
    chkMilliseconds: TRadioButton;
    numExpanderReducer: TUWNumberBox;
    numLongerCharacters: TUWNumberBox;
    numMilliseconds: TUWNumberBox;
    numSetMinimumDuration: TUWNumberBox;
    pagTimings: TPageControl;
    chkCharacters: TRadioButton;
    rdoAllTheSubtitles: TRadioButton;
    rdoOnlyMarkedSubtitles: TRadioButton;
    rdoOnlySelectedSubtitles: TRadioButton;
    rdoFromTheSelectedSubtitle: TRadioButton;
    tabDurationLimits: TTabSheet;
    numSetMaximumDuration: TUWNumberBox;
    tabAutomaticDurations: TTabSheet;
    numPerCharacter: TUWNumberBox;
    numPerWord: TUWNumberBox;
    numPerLine: TUWNumberBox;
    numPerSecond: TUWNumberBox;
    tabDurationExpanderReducer: TTabSheet;
    tabDelay: TTabSheet;
    tedDelay: TUWTimeEdit;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pagTimingsChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTimings: TfrmTimings;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain, UWSubtitleAPI, UWSubtitles.Utils, UWSystem.StrUtils,
  jsonConf;

{$R *.lfm}

// -----------------------------------------------------------------------------

procedure ApplyTimings(const Item: PUWSubtitleItem; const Index: Integer);
begin
  with Item^, frmTimings do
    case pagTimings.TabIndex of
      0: begin //Duration limits
           SetSubtitleTime(Index, SetDurationLimits(Subtitles.Duration[Index], numSetMaximumDuration.Value, numSetMinimumDuration.Value), frmMain.tedDuration.Tag, False, False);
         end;
      1: begin //Automatic durations
           if chkMilliseconds.Checked then
             SetSubtitleTime(Index, AutomaticDurations(Text, Subtitles.Duration[Index], numPerCharacter.Value, numPerWord.Value, numPerLine.Value, TAutomaticDurationMode(cboCondition.ItemIndex)), frmMain.tedDuration.Tag, False, False)
           else
             SetSubtitleTime(Index, AutomaticDurations(Text, Subtitles.Duration[Index], numPerSecond.Value, TAutomaticDurationMode(cboCondition.ItemIndex)), frmMain.tedDuration.Tag, False, False);
         end;
      2: begin //Duration expander/reducer
           SetSubtitleTime(Index, TimeExpander(Text, Subtitles.Duration[Index], numExpanderReducer.Value, iff(chkOnlyIfSubtitleLongerThan.Checked, numLongerCharacters.Value, 0), iff(chkOnlyIfDurationLongerThan.Checked, numMilliseconds.Value, 0), cboExpanderReducer.ItemIndex = 0), frmMain.tedDuration.Tag, False, False);
         end;
      3: begin //Delay
           SetSubtitleTime(Index, SetDelay(InitialTime, iff(cboDelay.ItemIndex = 0, tedDelay.Value, -tedDelay.Value)), frmMain.tedInitial.Tag, False, False);
           SetSubtitleTime(Index, SetDelay(FinalTime, iff(cboDelay.ItemIndex = 0, tedDelay.Value, -tedDelay.Value)), frmMain.tedFinal.Tag, False, False);
         end;
    end;
end;

// -----------------------------------------------------------------------------

{ TfrmTimings }

// -----------------------------------------------------------------------------

procedure TfrmTimings.FormCreate(Sender: TObject);
begin
  if frmMain.VST.SelectedCount = 1 then
    rdoFromTheSelectedSubtitle.Checked := True
  else if frmMain.VST.SelectedCount > 1 then
    rdoOnlySelectedSubtitles.Checked := True
  else
    rdoAllTheSubtitles.Checked := True;

  with TJSONConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    OpenKey('Timings', True);

    chkSetMaximumDuration.Checked := GetValue('chkSetMaximumDuration', True);
    chkSetMinimumDuration.Checked := GetValue('chkSetMinimumDuration', True);
    numSetMaximumDuration.Value   := GetValue('numSetMaximumDuration', 4000);
    numSetMinimumDuration.Value   := GetValue('numSetMinimumDuration', 800);

    cboCondition.ItemIndex  := GetValue('cboCondition', 0);
    chkCharacters.Checked   := GetValue('chkCharacters', False);
    chkMilliseconds.Checked := GetValue('chkMilliseconds', True);
    numPerSecond.Value      := GetValue('numPerSecond', 20);
    numPerCharacter.Value   := GetValue('numPerCharacter', 60);
    numPerWord.Value        := GetValue('numPerWord', 50);
    numPerLine.Value        := GetValue('numPerLine', 50);

    cboExpanderReducer.ItemIndex        := GetValue('cboExpanderReducer', 0);
    numExpanderReducer.Value            := GetValue('numExpanderReducer', 1500);
    chkOnlyIfSubtitleLongerThan.Checked := GetValue('chkOnlyIfSubtitleLongerThan', True);
    chkOnlyIfDurationLongerThan.Checked := GetValue('chkOnlyIfDurationLongerThan', True);
    numLongerCharacters.Value           := GetValue('numLongerCharacters', 40);
    numMilliseconds.Value               := GetValue('numMilliseconds', 1000);

    cboDelay.ItemIndex := GetValue('cboDelay', 0);
    tedDelay.Value     := GetValue('tedDelay', 0);

    CloseKey;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.FormShow(Sender: TObject);
begin
  ReadLangForForm(LanguageFileName, Self);
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with TJSONConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    OpenKey('Timings', True);

    SetValue('chkSetMaximumDuration', chkSetMaximumDuration.Checked);
    SetValue('chkSetMinimumDuration', chkSetMinimumDuration.Checked);
    SetValue('numSetMaximumDuration', numSetMaximumDuration.Value);
    SetValue('numSetMinimumDuration', numSetMinimumDuration.Value);

    SetValue('cboCondition', cboCondition.ItemIndex);
    SetValue('chkCharacters', chkCharacters.Checked);
    SetValue('chkMilliseconds', chkMilliseconds.Checked);
    SetValue('numPerSecond', numPerSecond.Value);
    SetValue('numPerCharacter', numPerCharacter.Value);
    SetValue('numPerWord', numPerWord.Value);
    SetValue('numPerLine', numPerLine.Value);

    SetValue('cboExpanderReducer', cboExpanderReducer.ItemIndex);
    SetValue('numExpanderReducer', numExpanderReducer.Value);
    SetValue('chkOnlyIfSubtitleLongerThan', chkOnlyIfSubtitleLongerThan.Checked);
    SetValue('chkOnlyIfDurationLongerThan', chkOnlyIfDurationLongerThan.Checked);
    SetValue('numLongerCharacters', numLongerCharacters.Value);
    SetValue('numMilliseconds', numMilliseconds.Value);

    SetValue('cboDelay', cboDelay.ItemIndex);
    SetValue('tedDelay', tedDelay.Value);

    CloseKey;
    Flush;
  finally
    Free;
  end;

  CloseAction := caFree;
  frmTimings := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.pagTimingsChange(Sender: TObject);
begin
  lyoOptions.Parent := pagTimings.Pages[pagTimings.PageIndex];
  {case pagTimings.TabIndex of
    0: lyoOptions.Parent := tabDurationLimits;
    1: lyoOptions.Parent := tabAutomaticDurations;
    2: lyoOptions.Parent := tabDurationExpanderReducer;
    3: lyoOptions.Parent := tabDelay;
  end;}
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmTimings.btnApplyClick(Sender: TObject);
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

  VSTDoLoop(@ApplyTimings, Selection);
  Close;
end;

// -----------------------------------------------------------------------------

end.

