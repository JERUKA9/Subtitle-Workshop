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

unit UMain;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, LCLIntf, Graphics, Dialogs,
  ComCtrls, ActnList, Menus, StdCtrls, Buttons, ExtCtrls, LCLType, UWControls,
  VirtualTrees, PasLibVlcPlayerUnit, PasLibVlcUnit, UUndo, UWSubtitleAPI, UTypes,
  UWSubtitleAPI.Formats, UWControls.WaveformDisplay, uPSComponent, uPSCompiler,
  uPSRuntime, uPSUtils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    aclActions: TActionList;
    actInsertSubtitle: TAction;
    actDeleteSubtitle: TAction;
    actInsertSubtitleBefore: TAction;
    actAudioPreview: TAction;
    actCut: TAction;
    actCopy: TAction;
    actAutomaticDuration: TAction;
    actDefaultPause: TAction;
    actAlignToNone: TAction;
    actAlignToLeft: TAction;
    actAlignToCenter: TAction;
    actAlignToRight: TAction;
    actFontBold: TAction;
    actFontItalic: TAction;
    actFontUnderline: TAction;
    actFontStrikeout: TAction;
    actFontClear: TAction;
    actFontColor: TAction;
    actFind: TAction;
    actFindNext: TAction;
    actActors: TAction;
    actExecuteExtension: TAction;
    actGoTo: TAction;
    actCloseSubtitle: TAction;
    actExit: TAction;
    actAbout: TAction;
    actDurationLimits: TAction;
    actAutomaticDurations: TAction;
    actDurationExpanderReducer: TAction;
    actDelay: TAction;
    actConvertCase: TAction;
    actAutoDivideSubtitles: TAction;
    actAutoCombineSubtitles: TAction;
    actCloseVideo: TAction;
    actLoadVideo: TAction;
    actSort: TAction;
    actTextFixPunctuation: TAction;
    actTextReverse: TAction;
    actTextSetMaxLineLength: TAction;
    actTextUnbreak: TAction;
    actTextAutobreak: TAction;
    actShiftTimeLess: TAction;
    actShiftTimeMore: TAction;
    actReadTextsFromFile: TAction;
    actReadTimingsFromFile: TAction;
    actViewToolbarAdditional: TAction;
    actViewChars: TAction;
    actViewStyle: TAction;
    actViewDuration: TAction;
    actViewTimes: TAction;
    actViewNumber: TAction;
    actSpellCheck: TAction;
    actSelectAll: TAction;
    actReplace: TAction;
    actQuickFind: TAction;
    actSubtitleDblClick: TAction;
    actStyles: TAction;
    actSimpleMode: TAction;
    actMediaAddSubtitle: TAction;
    actMediaPlayAfterSelection: TAction;
    actMediaPlayBeforeSelection: TAction;
    actMediaChangePlayRate: TAction;
    actMediaZoomSelection: TAction;
    actMediaZoomOut: TAction;
    actMediaZoomIn: TAction;
    actMediaPlayFromSelection: TAction;
    actMediaPlaySelection: TAction;
    actMediaEndSubtitle: TAction;
    actMediaStartSubtitle: TAction;
    actMediaSetFinalTime: TAction;
    actMediaSetInitialTime: TAction;
    actMediaForwardEx: TAction;
    actMediaRewindEx: TAction;
    actMediaNextFrame: TAction;
    actMediaForward: TAction;
    actMediaRewind: TAction;
    actShiftToNext: TAction;
    actShiftToPrevious: TAction;
    actNextSubtitle: TAction;
    actPreviousSubtitle: TAction;
    actMediaAutoScroll: TAction;
    actMediaStop: TAction;
    actMediaPlay: TAction;
    actNewSubtitle: TAction;
    actPaste: TAction;
    actSaveSubtitleAs: TAction;
    actLoadSubtitle: TAction;
    actVideoPreview: TAction;
    actUnMarkSubtitle: TAction;
    actMarkSubtitle: TAction;
    actRedo: TAction;
    actUndo: TAction;
    actTranslatorMode: TAction;
    actModeFrames: TAction;
    actModeTime: TAction;
    cboFind: TComboBox;
    cboStyle: TComboBox;
    cboActor: TComboBox;
    cboInputFPS: TComboBox;
    cboFPS: TComboBox;
    cboFormat: TComboBox;
    cboEncoding: TComboBox;
    cpsText: TUWCPSBar;
    cpsTranslation: TUWCPSBar;
    imgFind: TImage;
    imlMain: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblMediaTime: TLabel;
    lblText: TLabel;
    lblTranslation: TLabel;
    MenuItem1: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    mnuLanguage: TMenuItem;
    mnuFormat: TMenuItem;
    mnuSubtitles: TMenuItem;
    mnuRTL: TMenuItem;
    mnuTexts: TMenuItem;
    mnuTimings: TMenuItem;
    mnuToolbars: TMenuItem;
    mnuColumns: TMenuItem;
    mnuHelp: TMenuItem;
    mnuVideo: TMenuItem;
    mnuView: TMenuItem;
    mnuTools: TMenuItem;
    mnuFind: TMenuItem;
    mnuEdit: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    mnuDictionary: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuMemoX: TMenuItem;
    mnuMain: TMainMenu;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mmoText: TMemo;
    mnuFile: TMenuItem;
    mmoTranslation: TMemo;
    numTop: TUWNumberBox;
    numRight: TUWNumberBox;
    numBottom: TUWNumberBox;
    popMRU: TPopupMenu;
    popExtensions: TPopupMenu;
    popMemo: TPopupMenu;
    popVST: TPopupMenu;
    psExtensions: TPSScript;
    tlbVideoControls: TToolBar;
    tlbWaveControls: TToolBar;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    sbrSeek: TUWSeekBar;
    ToolButton23: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    ToolButton45: TToolButton;
    ToolButton46: TToolButton;
    ToolButton47: TToolButton;
    ToolButton48: TToolButton;
    ToolButton49: TToolButton;
    ToolButton50: TToolButton;
    ToolButton51: TToolButton;
    ToolButton52: TToolButton;
    ToolButton53: TToolButton;
    ToolButton54: TToolButton;
    ToolButton55: TToolButton;
    ToolButton56: TToolButton;
    ToolButton57: TToolButton;
    ToolButton58: TToolButton;
    ttTimes: TUWTickTime;
    WAVE: TUWWaveformDisplay;
    VLC: TPasLibVlcPlayer;
    popMenuInsert: TPopupMenu;
    shpColor1: TShape;
    shpColor2: TShape;
    shpColor3: TShape;
    shpColor4: TShape;
    shpColor5: TShape;
    shpColor6: TShape;
    shpColor7: TShape;
    shpColor8: TShape;
    spdInitial: TSpeedButton;
    spdFinal: TSpeedButton;
    spdDuration: TSpeedButton;
    spdPause: TSpeedButton;
    spdStyle: TSpeedButton;
    spdActor: TSpeedButton;
    sptVideo: TSplitter;
    stbStatus: TStatusBar;
    tedFinal: TUWTimeEdit;
    tedDuration: TUWTimeEdit;
    tedPause: TUWTimeEdit;
    tlbEditor1: TToolBar;
    tlbMain: TToolBar;
    tlbEditor: TToolBar;
    tlbExtra: TToolBar;
    ToolButton1: TToolButton;
    lyoEditor: TUWLayout;
    lyoSubtitles: TUWLayout;
    lyoVideo: TUWLayout;
    lyoEditorLeftPanel: TUWLayout;
    lyoEditorTopPanel: TUWLayout;
    tedInitial: TUWTimeEdit;
    lyoEditorClient: TUWLayout;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton35: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    numLeft: TUWNumberBox;
    lyoVideoControls: TUWLayout;
    VST: TVirtualStringTree;
    procedure actAboutExecute(Sender: TObject);
    procedure actAlignToCenterExecute(Sender: TObject);
    procedure actAlignToLeftExecute(Sender: TObject);
    procedure actAlignToNoneExecute(Sender: TObject);
    procedure actAlignToRightExecute(Sender: TObject);
    procedure actAudioPreviewExecute(Sender: TObject);
    procedure actAutoCombineSubtitlesExecute(Sender: TObject);
    procedure actAutoDivideSubtitlesExecute(Sender: TObject);
    procedure actAutomaticDurationExecute(Sender: TObject);
    procedure actAutomaticDurationsExecute(Sender: TObject);
    procedure actCloseSubtitleExecute(Sender: TObject);
    procedure actCloseVideoExecute(Sender: TObject);
    procedure actConvertCaseExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actDefaultPauseExecute(Sender: TObject);
    procedure actDelayExecute(Sender: TObject);
    procedure actDeleteSubtitleExecute(Sender: TObject);
    procedure actDurationExpanderReducerExecute(Sender: TObject);
    procedure actDurationLimitsExecute(Sender: TObject);
    procedure actExecuteExtensionExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actFontBoldExecute(Sender: TObject);
    procedure actFontClearExecute(Sender: TObject);
    procedure actFontColorExecute(Sender: TObject);
    procedure actFontItalicExecute(Sender: TObject);
    procedure actFontStrikeoutExecute(Sender: TObject);
    procedure actFontUnderlineExecute(Sender: TObject);
    procedure actGoToExecute(Sender: TObject);
    procedure actInsertSubtitleBeforeExecute(Sender: TObject);
    procedure actInsertSubtitleExecute(Sender: TObject);
    procedure actLoadSubtitleExecute(Sender: TObject);
    procedure actLoadVideoExecute(Sender: TObject);
    procedure actMarkSubtitleExecute(Sender: TObject);
    procedure actMediaAddSubtitleExecute(Sender: TObject);
    procedure actMediaChangePlayRateExecute(Sender: TObject);
    procedure actMediaAutoScrollExecute(Sender: TObject);
    procedure actMediaEndSubtitleExecute(Sender: TObject);
    procedure actMediaForwardExecute(Sender: TObject);
    procedure actMediaForwardExExecute(Sender: TObject);
    procedure actMediaPlayAfterSelectionExecute(Sender: TObject);
    procedure actMediaPlayBeforeSelectionExecute(Sender: TObject);
    procedure actMediaPlayExecute(Sender: TObject);
    procedure actMediaPlayFromSelectionExecute(Sender: TObject);
    procedure actMediaPlaySelectionExecute(Sender: TObject);
    procedure actMediaRewindExecute(Sender: TObject);
    procedure actMediaNextFrameExecute(Sender: TObject);
    procedure actMediaRewindExExecute(Sender: TObject);
    procedure actMediaSetFinalTimeExecute(Sender: TObject);
    procedure actMediaSetInitialTimeExecute(Sender: TObject);
    procedure actMediaStartSubtitleExecute(Sender: TObject);
    procedure actMediaStopExecute(Sender: TObject);
    procedure actMediaZoomInExecute(Sender: TObject);
    procedure actMediaZoomOutExecute(Sender: TObject);
    procedure actMediaZoomSelectionExecute(Sender: TObject);
    procedure actModeFramesExecute(Sender: TObject);
    procedure actModeTimeExecute(Sender: TObject);
    procedure actNewSubtitleExecute(Sender: TObject);
    procedure actNextSubtitleExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPreviousSubtitleExecute(Sender: TObject);
    procedure actQuickFindExecute(Sender: TObject);
    procedure actReadTextsFromFileExecute(Sender: TObject);
    procedure actReadTimingsFromFileExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actSaveSubtitleAsExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actShiftTimeLessExecute(Sender: TObject);
    procedure actShiftTimeMoreExecute(Sender: TObject);
    procedure actShiftToNextExecute(Sender: TObject);
    procedure actShiftToPreviousExecute(Sender: TObject);
    procedure actSimpleModeExecute(Sender: TObject);
    procedure actSortExecute(Sender: TObject);
    procedure actSpellCheckExecute(Sender: TObject);
    procedure actSubtitleDblClickExecute(Sender: TObject);
    procedure actTextAutobreakExecute(Sender: TObject);
    procedure actTextFixPunctuationExecute(Sender: TObject);
    procedure actTextReverseExecute(Sender: TObject);
    procedure actTextSetMaxLineLengthExecute(Sender: TObject);
    procedure actTextUnbreakExecute(Sender: TObject);
    procedure actTranslatorModeExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actUnMarkSubtitleExecute(Sender: TObject);
    procedure actVideoPreviewExecute(Sender: TObject);
    procedure actViewCharsExecute(Sender: TObject);
    procedure actViewDurationExecute(Sender: TObject);
    procedure actViewNumberExecute(Sender: TObject);
    procedure actViewStyleExecute(Sender: TObject);
    procedure actViewTimesExecute(Sender: TObject);
    procedure actViewToolbarAdditionalExecute(Sender: TObject);
    procedure cboFindChange(Sender: TObject);
    procedure cboFPSSelect(Sender: TObject);
    procedure cboInputFPSKeyPress(Sender: TObject; var Key: char);
    procedure cboInputFPSSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoTextChange(Sender: TObject);
    procedure numLeftValueChange(Sender: TObject);
    procedure popMemoPopup(Sender: TObject);
    procedure psExtensionsCompile(Sender: TPSScript);
    procedure psExtensionsCompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure psExtensionsExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    function psExtensionsNeedFile(Sender: TObject;
      const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
    procedure sbrSeekMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpColor1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tedInitialTimeChange(Sender: TObject; const NewTime: Cardinal);
    procedure UndoChanged(const ChangeType: UUndo.TUndoChangeType);
    procedure VLCClick(Sender: TObject);
    procedure VLCMediaPlayerLengthChanged(Sender: TObject; time: Int64);
    procedure VLCMediaPlayerTimeChanged(Sender: TObject; time: Int64);
    procedure VLCMediaPlayerTitleChanged(Sender: TObject; title: Integer);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTDblClick(Sender: TObject);
    procedure VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const AText: String;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure VSTResize(Sender: TObject);
    procedure WAVESelectedSubtitleItem(Sender: TObject; const Index: Integer;
      const SubtitleItem: TUWSubtitleItem; const IsDynamic: Boolean);
    procedure WAVESelectedSubtitleItemChange(Sender: TObject);
    procedure WAVESelectedSubtitleItemChanged(Sender: TObject;
      const Index: Integer; const OldInitialTime, OldFinalTime: Integer;
      const NeedSort: Boolean);
    procedure WAVESelectionChange(Sender: TObject);
  private
    { private declarations }
    procedure UpdateStatusBar;
    procedure MRUItemClick(Sender: TObject);
    procedure HunspellItemClick(Sender: TObject);
    procedure ShowFindAndReplace(const TabIndex: Integer = 0);
    procedure ShowTimings(const TabIndex: Integer = 0);
    procedure ShowTexts(const TabIndex: Integer = 0);
    procedure ReadSubtitleValuesFromFile(const FileName: String; const FormatIndex: TUWSubtitleFormats = sfInvalid; const Mode: TShowSubtitles = ssText; const ATimes: Boolean = True);
  public
    { public declarations }
    procedure UpdateCPSAndTexts;
    procedure UpdateColorsInBoxes(const Index: Integer);
    procedure UpdateValues(const VSTInvalidate: Boolean = False);
    procedure EnableWorkArea(const Value: Boolean = True);
    procedure EnableVideoControls(const Value: Boolean = True);
    procedure EnableAudioControls(const Value: Boolean = True);
    function CloseSubtitle: Boolean;
    procedure LoadSubtitle(const FileName: String; AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
    procedure SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
    procedure OpenVideo(const FileName: String; const Pos: Int64 = 0);
    procedure OpenAudio(const FileName: String);
    procedure LanguageItemClick(Sender: TObject);
    procedure DictionaryItemClick(Sender: TObject);
    procedure ExtensionItemClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses UWSystem.TimeUtils, UWSystem.StrUtils, UWSystem.SysUtils, UWFiles.MRU,
  UWSystem.Encoding, UWSpellCheck.Hunspell, UWSystem.HTMLUtils,
  UWSubtitleAPI.Tags, UCommon, UExtensions, UFindAndReplace, UErrors,
  UWControls.Utils, USpellCheck, UAbout, UWelcome, UTimings, UTexts,
  UWSubtitles.Utils;

{$R *.lfm}

// -----------------------------------------------------------------------------

{ TfrmMain }

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCreate(Sender: TObject);
var
  s: String;
begin
  DoubleBuffered := True;

  // Initialize SubtitleAPI
  Subtitles := TUWSubtitles.Create;
  // Undo engine
  Undo := TUndo.Create;
  Undo.OnChange := @UndoChanged;
  // VST Node/Header size
  TextSize  := Canvas.TextExtent('W');
  VST.DefaultNodeHeight := (TextSize.cy*4);
  VST.Header.Columns[0].Width := (TextSize.cx*7);
  VST.Header.Columns[1].Width := (TextSize.cx*8);
  VST.Header.Columns[2].Width := (TextSize.cx*7);
  VST.Header.Columns[3].Width := (TextSize.cx*7);
  VST.Header.Columns[6].Width := (TextSize.cx*5);
  // Our FormatSettings
  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := FormatSettings.DecimalSeparator;
  // Settings
  DefaultSettings;
  LoadSettings;
  // Prepare combos and menus
  FillComboWithFPS(cboInputFPS);
  FillComboWithFPS(cboFPS);
  FillComboWithEncodings(cboEncoding);
  FillComboWithFormats(cboFormat);
  FillWithLanguages(mnuLanguage, NIL);
  FillWithDictionaries(mnuDictionary, NIL);
  FillMenuWithExtensions('', popExtensions);
  // Get FPS
  FPS.InputFPS := GetInputFPS;
  FPS.FPS      := GetFPS;
  // Language & Shortcuts
  ReadLangForForm(LanguageFileName, Self);
  ReadShortCuts(ShortCutFileName);
  // MRU
  MRU := TUWMRU.Create(popMRU);
  MRU.OnMRUItemClick := @MRUItemClick;
  MRU.LoadFromJSON(MRUFileName);
  // Hunspell
  {$IFDEF MSWINDOWS}
    {$IFDEF WIN32}
      s := 'libhunspellx86.dll';
    {$ELSE}
      s := 'libhunspellx64.dll';
    {$ENDIF}
  {$ELSE}
    {$IFDEF UNIX}
      {$IFDEF DARWIN} // mac
        {$IFDEF CPU32}
          s := 'libhunspellx86.dylib';
        {$ELSE}
          s := 'libhunspellx64.dylib';
        {$ENDIF}
      {$ELSE} // linux
        {$IFDEF CPU32}
          s := 'libhunspellx86.so';
        {$ELSE}
          s := 'libhunspellx64.so';
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  Hunspell := TUWHunspell.Create(GetCustomFilePath(s));
  Hunspell.LoadDictionary(DictionariesFolder+Options.HunspellLang+'.aff', DictionariesFolder+Options.HunspellLang+'.dic');
  //
  Caption := ProgramName;

  EnableWorkArea(False);
  EnableVideoControls(False);
  EnableAudioControls(False);
  // Link subtitles to WaveformDisplay
  WAVE.Subtitles := Subtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CloseSubtitle;
  if CanClose then
  begin
    // Save settings
    SaveSettings;
    // Hunspell
    if Assigned(Hunspell) then Hunspell.Free;
    // MRU
    MRU.SaveToJSON(MRUFileName);
    MRU.Free;
    // VST
    VST.RootNodeCount := 0;
    // Free Undo
    Undo.Free;
    // Unlink Subtitles
    WAVE.Subtitles := NIL;
    // Free SubtitleAPI
    Subtitles.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormShow(Sender: TObject);
begin
  CommandLineProcess;

  if Options.ShowWelcomeAtStartup and not VST.Enabled then
  begin
    if frmWelcome = NIL then
    begin
      frmWelcome := TfrmWelcome.Create(Application);
      frmWelcome.ShowModal;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormResize(Sender: TObject);
var
  Border, w: Integer;
begin
  Border := VST.Left;

  if lyoEditorLeftPanel.Visible then
    w  := (ClientWidth-lyoEditorLeftPanel.Width) - (Border*4)
  else
    w  := (ClientWidth)-(Border*4);

  if TranslatorMode then
  begin
    mmoText.Width := (w div 2);
    cpsText.Width := mmoText.Width;
    mmoTranslation.Width := mmoText.Width;
    mmoTranslation.Left := mmoText.Width+(Border*3);
    lblTranslation.Left := mmoTranslation.Left;
    cpsTranslation.Left := mmoTranslation.Left;
    cpsTranslation.Width := mmoTranslation.Width;
  end
  else
  begin
    mmoText.Width := w + (Border*2);
    cpsText.Width := mmoText.Width;
  end;

  stbStatus.Panels[1].Width := ClientWidth - stbStatus.Panels[0].Width - stbStatus.Panels[2].Width;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  Ext : String;
  i   : Integer;
begin
  Ext := LowerCase(ExtractFileExt(FileNames[0]));
  // video?
  for i := 0 to High(TVideoExts) do
    if Ext = TVideoExts[i] then
    begin
      OpenVideo(FileNames[0]);
      Exit;
    end;
  // audio?
  for i := 0 to High(TAudioExts) do
    if Ext = TAudioExts[i] then
    begin
      OpenAudio(FileNames[0]);
      Exit;
    end;
  // subtitle?
  if TranslatorMode and (Subtitles.Count > 0) then
    ReadSubtitleValuesFromFile(FileNames[0], sfInvalid, ssTranslation, False)
  else
    LoadSubtitle(FileNames[0]);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTResize(Sender: TObject);
var
  wCols: Integer;
begin
  wCols := 0;
  if (coVisible in VST.Header.Columns[0].Options) then wCols := wCols + VST.Header.Columns[0].Width;
  if (coVisible in VST.Header.Columns[1].Options) then wCols := wCols + VST.Header.Columns[1].Width;
  if (coVisible in VST.Header.Columns[2].Options) then wCols := wCols + VST.Header.Columns[2].Width;
  if (coVisible in VST.Header.Columns[3].Options) then wCols := wCols + VST.Header.Columns[3].Width;
  if (coVisible in VST.Header.Columns[6].Options) then wCols := wCols + VST.Header.Columns[6].Width;
  wCols := (VST.Width-wCols) - (GetSystemMetrics(SM_CXVSCROLL)+5);
  if TranslatorMode then wCols := wCols div 2;
  VST.Header.Columns[4].Width := wCols;
  VST.Header.Columns[5].Width := wCols;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const AText: String;
  const CellRect: TRect; var DefaultDraw: Boolean);

var
  TS : TTextStyle;
  R  : TRect;
  c  : TColor;

  procedure DrawDiffDuration;
  var
    dur,
    doptimal : Cardinal;
    s        : String;
    cc       : TColor;
  begin
    TS.Alignment := taLeftJustify;
    doptimal := CalculateOptimalDisplayMS(Subtitles[Node^.Index].Text);
    dur := Subtitles.Duration[Node^.Index];
    s   := '';
    if (doptimal > dur) then
    begin
      dur := doptimal - dur;
      if dur > 1000 then
      begin
        s  := '-';
        cc := $0000FF;
      end;
    end
    else if (doptimal < dur) then
    begin
      dur := dur - doptimal;
      if dur > 1000 then
      begin
        s  := '+';
        cc := $FF0000;
      end;
    end;

    if s <> '' then
    begin
      if not (vsSelected in Node^.States) then
      begin
        c := TargetCanvas.Font.Color;
        TargetCanvas.Font.Color := cc;
      end;

      //R.Bottom := TextSize.cy*3;
      if WorkMode = wmTime then
        TargetCanvas.TextRect(R, R.Left, R.Top, s+TrimTimeString(TimeToString(dur, 'ss.z')), TS)
      else
        TargetCanvas.TextRect(R, R.Left, R.Top, s+IntToStr(TimeToFrames(dur, GetInputFPS)), TS);

      if not (vsSelected in Node^.States) then TargetCanvas.Font.Color := c;
    end;
  end;

  function GetColorCPS(const CPS: Double; const Default: TColor): TColor;
  begin
    if      (CPS < 13) then Result := $0000FF
    else if (CPS < 27) then Result := Default
    else                    Result := $FF0000;
  end;

  procedure DrawCPS(const Original: Boolean);
  var
    cps: Double;
  begin
    if Original then
      cps := Subtitles.TextCPS[Node^.Index]
    else
      cps := Subtitles.TranslationCPS[Node^.Index];

    if not (vsSelected in Node^.States) then
    begin
      c := TargetCanvas.Font.Color;
      TargetCanvas.Font.Color := GetColorCPS(cps, c);
    end;
    //if c <> TargetCanvas.Font.Color then
    //  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold]
    //else
    //  TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
    TargetCanvas.TextRect(R, R.Left, R.Top, Format('%.2f cps', [cps], FormatSettings), TS);
    if not (vsSelected in Node^.States) then TargetCanvas.Font.Color := c;
  end;

begin
  DefaultDraw := False;
  if vsSelected in Node^.States then TargetCanvas.Font.Color := clWhite;
  TS.SingleLine := False;
  R := CellRect;

  case Column of
    0: begin
         TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
         TS.Layout := tlTop;
         // !
         if Subtitles[Node^.Index].ErrorType <> [] then
         begin
           c := TargetCanvas.Font.Color;
           TargetCanvas.Font.Color := clRed;
           TS.Alignment := taLeftJustify;
           TargetCanvas.TextRect(R, R.Left, R.Top, '!', TS);
           TargetCanvas.Font.Color := c;
         end;
         // #
         TS.Alignment := taRightJustify;
         TargetCanvas.TextRect(R, R.Left, R.Top, '#' + IntToStr(Node^.Index+1), TS);
         // Pause
         R.Top := TextSize.cy;
         TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
         TargetCanvas.TextRect(R, R.Left, R.Top, GetPauseTimeStr(Node^.Index, True), TS);
         // CPS
         R.Top := TextSize.cy*2; // text cps
         DrawCPS(True);
         if TranslatorMode then
         begin
           R.Top := TextSize.cy*3; // translation cps
           DrawCPS(False);
         end;
       end;
    1: begin
         // Times
         TS.Alignment := taCenter;
         TS.Layout := tlTop;
         TargetCanvas.TextRect(R, R.Left, R.Top, GetInitialTimeStr(Node^.Index) + sLineBreak +
           GetFinalTimeStr(Node^.Index), TS);
       end;
    2: begin
         // Duration
         TS.Alignment := taRightJustify;
         TS.Layout := tlBottom;
         TargetCanvas.TextRect(R, R.Left, R.Top, GetDurationTimeStr(Node^.Index, True), TS);
         DrawDiffDuration; // diff text
       end;
    3: begin
         // Style/Actor
       end;
    4: begin
         // Text
         if Options.DrawTags then
           DrawHTMLTextEx(TargetCanvas, R, Subtitles[Node^.Index].Text,
           Application.BidiMode <> bdLeftToRight, swt_StartTag, swt_EndTag)
         else
         begin
           TS.Alignment := taLeftJustify;
           TS.Layout := tlTop;
           //TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
           TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Text, TS);
         end;
       end;
    5: begin
         // Translation
         if Options.DrawTags then
           DrawHTMLTextEx(TargetCanvas, R, Subtitles[Node^.Index].Translation,
           Application.BidiMode <> bdLeftToRight, swt_StartTag, swt_EndTag)
         else
         begin
           TS.Alignment := taLeftJustify;
           TS.Layout := tlTop;
           //TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
           TargetCanvas.TextRect(R, R.Left, R.Top, Subtitles[Node^.Index].Translation, TS);
         end;
       end;
    6: begin
         // Chars
         TS.Alignment := taRightJustify;
         TS.Layout := tlTop;
         c := TargetCanvas.Font.Color;
         if (etBreakLongLines in Subtitles[Node^.Index].ErrorType) and not (vsSelected in Node^.States) then TargetCanvas.Font.Color := clRed;
         if TranslatorMode then
         begin
           TargetCanvas.TextRect(R, R.Left, R.Top, GetLengthForEachLine(Subtitles[Node^.Index].Translation), TS);
           TS.Alignment := taLeftJustify;
         end;
         TargetCanvas.TextRect(R, R.Left, R.Top, GetLengthForEachLine(Subtitles[Node^.Index].Text), TS);
         TargetCanvas.Font.Color := c;
       end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  SelColor: TColor;
begin
  SelColor := TargetCanvas.Brush.Color;

  // row background
  if (Column = 5) or (Node^.Index mod 2 = 0) then
    VSTPaintCell(TargetCanvas, CellRect, $00E5E5E5);

  // selection
  if vsSelected in Node^.States then
  begin
    SelColor := MixColors(VST.Colors.FocusedSelectionColor, SelColor, iff(VST.Focused, 70, 40));
    VSTPaintCell(TargetCanvas, CellRect, SelColor);
  end;

  // errors
  if (etOverlapping in Subtitles[Node^.Index].ErrorType) and (Column in [0..1]) then
    VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.Overlapping));
  if (etBadValues in Subtitles[Node^.Index].ErrorType) and (Column = 1) then
    VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.BadValues));
  if (etTimeTooShort in Subtitles[Node^.Index].ErrorType) and (Column = 2) then
    VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.TimeTooShort))
  else if (etTimeTooLong in Subtitles[Node^.Index].ErrorType) and (Column = 2) then
    VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.TimeTooLong));
  if (etPauseTooShort in Subtitles[Node^.Index].ErrorType) and (Column = 0) then
    VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.PauseTooShort));

  // untranslated text
  if (Column = 5) and TranslatorMode and ((Subtitles[Node^.Index].Text = Subtitles[Node^.Index].Translation) or (Subtitles[Node^.Index].Translation = '')) then
    VSTPaintCell(TargetCanvas, CellRect, MixColors(SelColor, Options.Colors.Untranslated));

  // marked
  if (Column = 0) and Subtitles[Node^.Index].Marked then
    VSTPaintCell(TargetCanvas, Rect(CellRect.Left, CellRect.Top, CellRect.Left+2, CellRect.Bottom), clBlue);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: String);

  procedure SetHintText(const S: String);
  begin
    if HintText = '' then
      HintText := S
    else
      HintText := HintText + sLineBreak + S;
  end;

begin
  LineBreakStyle := hlbForceSingleLine;
  HintText := '';
  with Subtitles[Node^.Index] do
    if ErrorType = [] then
    begin
      if Marked then SetHintText(ErrorStrings.Marked);
    end
    else
    begin
      if etBadValues in ErrorType then SetHintText(ErrorStrings.BadValues);
      if etTimeTooLong in ErrorType then SetHintText(ErrorStrings.TimeTooLong);
      if etTimeTooShort in ErrorType then SetHintText(ErrorStrings.TimeTooShort);
      if etPauseTooShort in ErrorType then SetHintText(ErrorStrings.PauseTooShort);
      if etMaxCPS in ErrorType then SetHintText(ErrorStrings.MaxCPS);
      if etOverlapping in ErrorType then SetHintText(ErrorStrings.Overlapping);
      if etFixTags in ErrorType then SetHintText(ErrorStrings.FixTags);
      if etEmpty in ErrorType then SetHintText(ErrorStrings.Empty);
      if etUnnecessarySpaces in ErrorType then SetHintText(ErrorStrings.UnnecessarySpaces);
      if etUnnecessaryDots in ErrorType then SetHintText(ErrorStrings.UnnecessaryDots);
      if etRepeatedChars in ErrorType then SetHintText(ErrorStrings.RepeatedChars);
      if etProhibitedChars in ErrorType then SetHintText(ErrorStrings.ProhibitedChars);
      if etHearingImpaired in ErrorType then SetHintText(ErrorStrings.HearingImpaired);
      if etBreakLongLines in ErrorType then SetHintText(ErrorStrings.BreakLongLines);
      if etRepeatedSubtitle in ErrorType then SetHintText(ErrorStrings.RepeatedSubtitle);
      if etOCR in ErrorType then SetHintText(ErrorStrings.OCR);
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VSTDblClick(Sender: TObject);

  procedure GoToFocusedSubtitleTime(const Before: Boolean = False; const Value: Byte = 1);
  var
    n: Integer;
  begin
    n := VSTFocusedNode;
    if n >= 0 then
    begin
      if not Before then
        VLCSeekTo(Subtitles[n].InitialTime)
      else
        VLCSeekTo(Subtitles[n].InitialTime - (Value * 1000));
    end;
  end;

begin
  if VLC.GetVideoLenInMs > 0 then
  begin
    if IsShiftKeyPressed then
    begin
      case Options.ShiftDblClickMode of
        dcmFocusTextBox          : FocusMemo;
        dcmGoSubtitleTime        : GoToFocusedSubtitleTime;
        dcmGoNBeforeSubtitleTime : GoToFocusedSubtitleTime(True, Options.ShiftDblClickModeValue);
      end;
    end
    else
    begin
      case Options.DblClickMode of
        dcmFocusTextBox          : FocusMemo;
        dcmGoSubtitleTime        : GoToFocusedSubtitleTime;
        dcmGoNBeforeSubtitleTime : GoToFocusedSubtitleTime(True, Options.DblClickModeValue);
      end;
    end;
  end
  else
  begin
    if WAVE.IsPeakDataLoaded then
      WAVE.SelectSubtitle(VSTFocusedNode, True, True);

    FocusMemo;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboInputFPSKeyPress(Sender: TObject; var Key: char);
var
  CBO: TComboBox;
begin
  CBO := (Sender as TComboBox);
  if Key = ',' then Key := '.';
  if (Key = Chr(VK_RETURN)) and IsFloat(CBO.Text, FormatSettings) then
    AddFPSToCombo(SysUtils.StrToFloat(CBO.Text, FormatSettings), CBO)
  else if not CharInSet(Key, ['0'..'9', '.', Chr(VK_BACK)]) or
    (Key = '.') and (StringCount('.', CBO.Text) = 1) then
    Key := #0;
end;

procedure TfrmMain.cboInputFPSSelect(Sender: TObject);
begin
  VST.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboFPSSelect(Sender: TObject);
begin
  VSTDoLoop(@ApplyChangeFPS, dlAll, False);
  FPS.FPS := GetFPS;
  VST.Invalidate;
  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.tedInitialTimeChange(Sender: TObject; const NewTime: Cardinal);
begin
  if not VSTUpdating and (VSTFocusedNode > -1) then
    with (Sender as TUWTimeEdit) do
    begin
      if VST.SelectedCount = 1 then
        SetSubtitleTime(VSTFocusedNode, CorrectTime(NewTime), Tag)
      else
        case Tag of
          0 : VSTDoLoop(@ApplySetTimeInitialFromSpin, dlSelected, True);
          1 : VSTDoLoop(@ApplySetTimeFinalFromSpin, dlSelected, True);
          2 : VSTDoLoop(@ApplySetTimeDurationFromSpin, dlSelected, True);
          3 : VSTDoLoop(@ApplySetTimePauseFromSpin, dlSelected, True);
        end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.mmoTextChange(Sender: TObject);
begin
  if not VSTUpdating and (VSTFocusedNode > -1) then
    with (Sender as TMemo) do
      if VST.SelectedCount = 1 then
        SetSubtitleText(VSTFocusedNode, Text, Tag = 0)
      else
        case Tag of
          0 : VSTDoLoop(@ApplySetTextFromEdit, dlSelected, True);
          1 : VSTDoLoop(@ApplySetTranslationFromEdit, dlSelected, True);
        end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.cboFindChange(Sender: TObject);
begin
  if VLC.IsPlay() and actMediaAutoScroll.Checked then actMediaAutoScroll.Checked := False;
  if not VSTFind(cboFind.Text, False, True) then VST.ClearSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.numLeftValueChange(Sender: TObject);
begin
  if (VST.SelectedCount > 1) then
    VSTDoLoop(@ApplyXY)
  else
    ApplyXY(Subtitles.ItemPointer[VSTFocusedNode], VSTFocusedNode);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.sbrSeekMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  VLCSeekTo(sbrSeek.Position);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.shpColor1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    LastSubtitle.Color := (Sender as TShape).Brush.Color;
    actFontColor.Execute;
  end
  else if Button = TMouseButton.mbRight then
    with TColorDialog.Create(Self) do
    try
      if Execute then
      begin
        LastSubtitle.Color := Color;
        (Sender as TShape).Brush.Color := Color;
        actFontColor.Execute;
      end;
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UndoChanged(const ChangeType: UUndo.TUndoChangeType);
begin
  actUndo.Enabled := Undo.CanUndo;
  actRedo.Enabled := Undo.CanRedo;

  if ChangeType = uctReIndex then
  begin
    VST.RootNodeCount := Subtitles.Count;
    UpdateValues(True);
  end
  else if ChangeType = uctItems then UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateStatusBar;
var
  s: String;
begin
  s := '';
  if VST.Enabled then
  begin
    if VST.SelectedCount > 1 then
      s := Format(Strings.LineSelected, [VST.SelectedCount, VST.TotalCount])
    else if (VST.SelectedCount = 1) and (VSTFocusedNode >= 0) then
      s := Format('%d / %d', [VSTFocusedNode+1, VST.TotalCount])
    else
      s := IntToStr(VST.TotalCount);
  end;
  stbStatus.Panels[2].Text := s;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateCPSAndTexts;
begin
  if (VSTFocusedNode > -1) and (VST.SelectedCount = 1) then
  begin
    lblText.Caption := Format(Strings.TextChars, [GetLengthForEachLine(Subtitles[VSTFocusedNode].Text, '/', '=')], FormatSettings);
    lblTranslation.Caption := Format(Strings.TranslationChars, [GetLengthForEachLine(Subtitles[VSTFocusedNode].Translation, '/', '=')], FormatSettings);
    cpsText.SetCPS(Subtitles.TextCPS[VSTFocusedNode]);
    cpsTranslation.SetCPS(Subtitles.TranslationCPS[VSTFocusedNode]);
  end
  else
  begin
    lblText.Caption := Strings.Text;
    lblTranslation.Caption := Strings.Translation;
    cpsText.SetCPS(0);
    cpsTranslation.SetCPS(0);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateColorsInBoxes(const Index: Integer);
begin
  if VST.SelectedCount > 1 then // multiple selected? paint disabled colors
  begin
    tedInitial.Color     := clBtnFace;
    tedFinal.Color       := tedInitial.Color;
    tedDuration.Color    := tedInitial.Color;
    tedPause.Color       := tedInitial.Color;
    mmoText.Color        := tedInitial.Color;
    mmoTranslation.Color := tedInitial.Color;
  end
  else
    // error colors
    with Subtitles[Index] do
    begin
      mmoText.Color        := clDefault;
      mmoTranslation.Color := mmoText.Color;

      if (etOverlapping in ErrorType) then
        tedInitial.Color := MixColors(clWhite, Options.Colors.Overlapping)
      else if (etBadValues in ErrorType) then
      begin
        tedInitial.Color := MixColors(clWhite, Options.Colors.BadValues);
        tedFinal.Color   := tedInitial.Color;
      end
      else
      begin
        tedInitial.Color := clDefault;
        tedFinal.Color   := clDefault;
      end;

      if (etTimeTooShort in ErrorType) then
        tedDuration.Color := MixColors(clWhite, Options.Colors.TimeTooShort)
      else if (etTimeTooLong in ErrorType) then
        tedDuration.Color := MixColors(clWhite, Options.Colors.TimeTooLong)
      else
        tedDuration.Color := clDefault;

      if (etPauseTooShort in ErrorType) then
        tedPause.Color := MixColors(clWhite, Options.Colors.PauseTooShort)
      else
        tedPause.Color := clDefault;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.UpdateValues(const VSTInvalidate: Boolean = False);
var
  NodeIndex: Integer;
  LastIndex: Integer;
begin
  VSTBeginUpdate;
  try
    if VST.SelectedCount = 0 then // zero selected
    begin
      mmoText.Text        := '';
      mmoTranslation.Text := '';
      tedInitial.SetValueOnly(0);
      tedFinal.SetValueOnly(0);
      tedDuration.SetValueOnly(0);
      tedPause.SetValueOnly(0);
      actShiftToPrevious.Enabled   := False;
      actDefaultPause.Enabled      := False;
      actShiftToNext.Enabled       := False;
      actAutomaticDuration.Enabled := False;
      numLeft.SetValueOnly(0);
      numTop.SetValueOnly(0);
      numRight.SetValueOnly(0);
      numBottom.SetValueOnly(0);
    end
    else if Assigned(VST.GetFirstSelected) then // one or more selected
    begin
      LastIndex := -1;
      NodeIndex := VST.GetFirstSelected^.Index;
      if VST.SelectedCount > 1 then LastIndex := VSTLastSelectedNode;
      if not Subtitles.ValidIndex(LastIndex) then
        LastIndex := NodeIndex;

      tedInitial.SetValueOnly(GetInitialTime(NodeIndex));
      tedFinal.SetValueOnly(GetFinalTime(LastIndex));
      tedPause.SetValueOnly(GetPauseTime(NodeIndex));
      if VST.SelectedCount = 1 then
      begin
        tedDuration.SetValueOnly(GetDurationTime(NodeIndex));
        mmoText.Text        := Subtitles[NodeIndex].Text;
        mmoTranslation.Text := Subtitles[NodeIndex].Translation;
        case Subtitles[NodeIndex].Align of
          0: actAlignToNone.Checked   := True;
          1: actAlignToLeft.Checked   := True;
          2: actAlignToCenter.Checked := True;
          3: actAlignToRight.Checked  := True;
        end;
        actShiftToPrevious.Enabled := (NodeIndex > 0);
        actDefaultPause.Enabled    := actShiftToPrevious.Enabled;
        actShiftToNext.Enabled     := NodeIndex < Subtitles.Count-1;
        numLeft.SetValueOnly(Subtitles[NodeIndex].R.Left);
        numTop.SetValueOnly(Subtitles[NodeIndex].R.Top);
        numRight.SetValueOnly(Subtitles[NodeIndex].R.Right);
        numBottom.SetValueOnly(Subtitles[NodeIndex].R.Bottom);
      end
      else
      begin
        tedDuration.SetValueOnly(Range(tedFinal.Value - tedInitial.Value, 0, tedFinal.Value));
        mmoText.Text        := '';
        mmoTranslation.Text := '';
        actAlignToNone.Checked   := False;
        actAlignToLeft.Checked   := False;
        actAlignToCenter.Checked := False;
        actAlignToRight.Checked  := False;

        actShiftToPrevious.Enabled := True;
        actDefaultPause.Enabled    := True;
        actShiftToNext.Enabled     := True;
        numLeft.SetValueOnly(0);
        numTop.SetValueOnly(0);
        numRight.SetValueOnly(0);
        numBottom.SetValueOnly(0);
      end;
      actAutomaticDuration.Enabled := True;

      UpdateCPSAndTexts;
      UpdateColorsInBoxes(NodeIndex);
    end;
  finally
    VSTEndUpdate;
  end;

  if VSTInvalidate then VST.Invalidate;
  UpdateStatusBar;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.EnableWorkArea(const Value: Boolean = True);
begin
  // Components
  VST.Enabled            := Value;
  cboFormat.Enabled      := Value;
  cboEncoding.Enabled    := Value;
  cboFPS.Enabled         := Value;
  mmoText.Enabled        := Value;
  mmoTranslation.Enabled := Value;
  tedInitial.Enabled     := Value;
  tedFinal.Enabled       := Value;
  tedDuration.Enabled    := Value;
  tedPause.Enabled       := Value;
  spdInitial.Enabled     := Value;
  spdFinal.Enabled       := Value;
  spdDuration.Enabled    := Value;
  spdPause.Enabled       := Value;
  numLeft.Enabled        := Value;
  numTop.Enabled         := Value;
  numRight.Enabled       := Value;
  numBottom.Enabled      := Value;
  shpColor1.Enabled      := Value;
  shpColor2.Enabled      := Value;
  shpColor3.Enabled      := Value;
  shpColor4.Enabled      := Value;
  shpColor5.Enabled      := Value;
  shpColor6.Enabled      := Value;
  shpColor7.Enabled      := Value;
  shpColor8.Enabled      := Value;
  cboActor.Enabled       := Value;
  cboStyle.Enabled       := Value;
  //
  if VST.Enabled and mmoText.Enabled then
  begin
    VSTSelectNode(0, True);
    mmoText.SetFocus;
  end;
  // Menus
  EnableMenu(mnuTimings, Value);
  EnableMenu(mnuTexts, Value);
  EnableMenu(mnuSubtitles, Value);
  EnableMenu(mnuFormat, Value);
  // Actions
  actRedo.Enabled := False;
  actUndo.Enabled := False;
  EnableAction(actInsertSubtitle, Value);
  EnableAction(actDeleteSubtitle, Value);
  EnableAction(actInsertSubtitleBefore, Value);
  EnableAction(actSaveSubtitleAs, Value);
  EnableAction(actUnMarkSubtitle, Value);
  EnableAction(actMarkSubtitle, Value);
  EnableAction(actNextSubtitle, Value);
  EnableAction(actPreviousSubtitle, Value);
  EnableAction(actCut, Value);
  EnableAction(actCopy, Value);
  EnableAction(actPaste, Value);
  EnableAction(actSelectAll, Value);
  EnableAction(actFind, Value);
   EnableAction(actFindNext, Value);
  EnableAction(actReplace, Value);
  EnableAction(actGoTo, Value);
  EnableAction(actSpellCheck, Value);
  EnableAction(actDurationLimits, Value);
  EnableAction(actAutomaticDurations, Value);
  EnableAction(actDurationExpanderReducer, Value);
  EnableAction(actDelay, Value);
  EnableAction(actConvertCase, Value);
  EnableAction(actReadTimingsFromFile, Value);
  EnableAction(actReadTextsFromFile, Value);
  EnableAction(actShiftTimeMore, Value);
  EnableAction(actShiftTimeLess, Value);
  EnableAction(actTextAutobreak, Value);
  EnableAction(actTextUnbreak, Value);
  EnableAction(actTextSetMaxLineLength, Value);
  EnableAction(actTextReverse, Value);
  EnableAction(actTextFixPunctuation, Value);
  EnableAction(actAutoDivideSubtitles, Value);
  EnableAction(actAutoCombineSubtitles, Value);
  EnableAction(actSort, Value);
  EnableAction(actFontBold, Value);
  EnableAction(actFontItalic, Value);
  EnableAction(actFontUnderline, Value);
  EnableAction(actFontStrikeout, Value);
  EnableAction(actFontClear, Value);
  EnableAction(actAlignToLeft, Value);
  EnableAction(actAlignToCenter, Value);
  EnableAction(actAlignToRight, Value);
  EnableAction(actAlignToNone, Value);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.EnableVideoControls(const Value: Boolean = True);
begin
  sbrSeek.Enabled := Value;
  EnableAction(actMediaPlay, Value);
  EnableAction(actMediaStop, Value);
  EnableAction(actMediaRewind, Value);
  EnableAction(actMediaForward, Value);
  EnableAction(actMediaNextFrame, Value);
  EnableAction(actMediaSetInitialTime, Value);
  EnableAction(actMediaSetFinalTime, Value);
  EnableAction(actMediaStartSubtitle, Value);
  EnableAction(actMediaEndSubtitle, Value);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.EnableAudioControls(const Value: Boolean = True);
begin
  EnableAction(actMediaPlaySelection, Value);
  EnableAction(actMediaPlayFromSelection, Value);
  EnableAction(actMediaPlayBeforeSelection, Value);
  EnableAction(actMediaPlayAfterSelection, Value);
  EnableAction(actMediaAddSubtitle, Value);
  EnableAction(actMediaZoomIn, Value);
  EnableAction(actMediaZoomOut, Value);
  EnableAction(actMediaZoomSelection, Value);
end;

// -----------------------------------------------------------------------------

function TfrmMain.CloseSubtitle: Boolean;
var
  r: Integer;
begin
  Result := False;

  if VST.Enabled then // ask to save if needed
  begin
    if SubtitleFile.Text.Changed then
    begin
      r := MsgSaveSubtitle(SubtitleFile.Text.FileName);
      case r of
        mrYes    : actSaveSubtitleAs.Execute;
        mrCancel : Abort;
      end;
    end;

    if actTranslatorMode.Checked then
    begin
      if SubtitleFile.Translation.Changed then
      begin
        r := MsgSaveSubtitle(SubtitleFile.Translation.FileName);
        case r of
          mrYes    : ;//actSubtitleSaveTranslationAs.Execute;
          mrCancel : Abort;
        end;
      end;
    end;
  end;

  SubtitleFile.Text.FileName := '';
  SubtitleFile.Translation.FileName := '';
  SubtitleChanged(False, False);
  WAVE.Close;
  VST.RootNodeCount := 0;
  Subtitles.Clear;
  EnableWorkArea(False);
  mmoText.Lines.Clear;
  mmoTranslation.Lines.Clear;
  UpdateCPSAndTexts;
  UpdateStatusBar;
  Caption := ProgramName;
  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.LoadSubtitle(const FileName: String; AFormat: TUWSubtitleFormats = sfInvalid; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS: Single;
begin
  if not CloseSubtitle then Exit;

  _FPS := AFPS;
  if _FPS = -1 then _FPS := FPS.InputFPS;
  Subtitles.LoadFromFile(FileName, AEncoding, _FPS, AFormat);
  VST.RootNodeCount := Subtitles.Count;
  cboEncoding.ItemIndex := GetEncodingIndex(Subtitles.CodePage);
  cboFormat.ItemIndex := Integer(Subtitles.Format)-1;

  if Options.AutoCheckErrors then
    VSTDoLoop(@ApplyCheckErrors, dlAll, False);

  SubtitleFile.Text.FileName := ExtractFileName(FileName);
  Caption := SubtitleFile.Text.FileName + ' - ' + ProgramName;
  EnableWorkArea;

  OpenVideo(MediaFileExists(FileName, TVideoExts));
  OpenAudio(MediaFileExists(FileName, TAudioExts));

  MRU.Add(FileName);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.SaveSubtitle(const FileName: String; const Format: TUWSubtitleFormats; const AEncoding: TEncoding = NIL; const AFPS: Single = -1);
var
  _FPS      : Single;
  _Encoding : TEncoding;
begin
  _FPS := AFPS;
  if _FPS = -1 then _FPS := FPS.InputFPS;
  _Encoding := AEncoding;
  if _Encoding = NIL then  _Encoding := TEncoding.GetEncoding(Encodings[cboEncoding.ItemIndex].CPID);

  Subtitles.SaveToFile(FileName, _FPS, _Encoding, Format);
  MRU.Add(FileName);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.OpenVideo(const FileName: String; const Pos: Int64 = 0);
begin
  if FileExists(FileName) and not VideoPreview then actVideoPreview.Execute;
  VLC.Play(FileName);
  actMediaChangePlayRateExecute(NIL);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.OpenAudio(const FileName: String);
begin
  WAVE.LoadWaveFromFile(FileName);
  if WAVE.IsPeakDataLoaded and not AudioPreview then
  begin
    EnableAudioControls(True);
    actAudioPreview.Execute;
    if (VLC.GetVideoLenInMs() <= 0) then OpenVideo(FileName);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.MRUItemClick(Sender: TObject);
begin
  LoadSubtitle((Sender as TMenuItem).Caption);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.popMemoPopup(Sender: TObject);
var
  Memo     : TMemo;
  Suggests : TStrings;
  i        : Integer;
  mnu      : TMenuItem;
  s        : String;
begin
  // check word under caret
  Memo     := GetMemoFocused;
  Suggests := NIL;

  if Memo = NIL then Exit;
  mnuMemoX.Visible := False;

  for i := popMemo.Items.Count-1 downto 0 do
    if Copy(popMemo.Items[i].Name, 1, 3) = 'hs_' then
      popMemo.Items[i].Free;

  s := Memo_GetWordUnderCaret(Memo);

  if Hunspell.Suggest(s, Suggests) then
  begin
    for i := 0 to Suggests.Count-1 do
    begin
      mnu         := TMenuItem.Create(popMemo);
      mnu.Name    := 'hs_' + IntToStr(i);
      mnu.Caption := Suggests[i];
      mnu.OnClick := @HunspellItemClick;
      popMemo.Items.Insert(i, mnu);
    end;
    mnuMemoX.Visible := True;
  end;

  if Suggests <> NIL then Suggests.Free;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.HunspellItemClick(Sender: TObject);
var
  Memo : TMemo;
begin
  // word suggest clicked
  Memo := GetMemoFocused;
  if Memo = NIL then Exit;

  if Memo.SelText = '' then Memo_GetWordUnderCaret(Memo, True);

  Memo.SelText   := (Sender as TMenuItem).Caption;
  Memo.SelLength := Length(Memo.SelText);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.LanguageItemClick(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  // select language
  s := GetDictNameFromCaption((Sender as TMenuItem).Caption);
  if Options.Language <> s then
  begin
    for i := 0 to mnuLanguage.Count-1 do mnuLanguage.Items[i].Checked := False;

    (Sender as TMenuItem).Checked := True;
    Options.Language := s;

    ReadLangForForm(LanguageFileName, Self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.DictionaryItemClick(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  // select dictionary
  s := GetDictNameFromCaption((Sender as TMenuItem).Caption);
  if Options.HunspellLang <> s then
  begin
    for i := 0 to mnuDictionary.Count-1 do mnuDictionary.Items[i].Checked := False;

    (Sender as TMenuItem).Checked := True;
    Options.HunspellLang := s;

    Hunspell.LoadDictionary(DictionariesFolder+Options.HunspellLang+'.aff', DictionariesFolder+Options.HunspellLang+'.dic');
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ExtensionItemClick(Sender: TObject);

  procedure OutputMsg(s: String);
  var
    l          : LongInt;
    ErrorsText : String;
  begin
    ErrorsText := '';
    for l := 0 to psExtensions.CompilerMessageCount - 1 do
    begin
      ErrorsText := ErrorsText + sLineBreak + psExtensions.CompilerErrorToStr(l);
    end;
    ShowMessage(s + sLineBreak + sLineBreak + ErrorsText);
  end;

var
  Item: TMenuItem;
  s: String;
begin
  with (Sender as TMenuItem) do
  begin
    s    := Caption;
    Item := Parent;
  end;
  while Item <> NIL do
  begin
    s    := ConcatPaths([Item.Caption, s]);
    Item := Item.Parent;
  end;
  s := ConcatPaths([ExtensionsFolder, s]) + '.pas';

  with TUWStringList.Create(s) do
  try
    psExtensions.MainFileName := s;
    psExtensions.Script.Text  := Text;
  finally
    Free;
  end;

  if psExtensions.Compile then
  begin
    if not psExtensions.Execute then
    begin
      ShowMessage(ErrorStrings.ExecuteScript + sLineBreak + sLineBreak +
        IntToStr(psExtensions.Exec.ExceptionPos) + '. ' +
        psExtensions.ExecErrorToString);
    end
    else
    begin
      if Options.AutoCheckErrors then
        VSTDoLoop(@ApplyCheckErrors, dlAll, False);
    end;
  end
  else
    OutputMsg(ErrorStrings.CompileScript);

  UpdateValues(True);
  IncrementUndoGroup;
end;

// -----------------------------------------------------------------------------

{ EXTENSIONS }

// -----------------------------------------------------------------------------

procedure TfrmMain.psExtensionsCompile(Sender: TPSScript);
begin
  psCompile(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psExtensionsCompImport(Sender: TObject; x: TPSPascalCompiler
  );
begin
  psCompImport(Sender, x);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.psExtensionsExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  psExecImport(Sender, se, x);
end;

// -----------------------------------------------------------------------------

function TfrmMain.psExtensionsNeedFile(Sender: TObject;
  const OrginFileName: tbtstring; var FileName, Output: tbtstring): Boolean;
var
  FS: TFileStream;
begin
  Result := False;

  try
    FS := TFileStream.Create(ExtractFilePath(OrginFileName) + FileName,
      fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Output, FS.Size);
    FS.Read(Output[1], Length(Output));
  finally
    FS.Free;
  end;

  Result := True;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ShowFindAndReplace(const TabIndex: Integer = 0);
begin
  if frmFindAndReplace = NIL then
  begin
    frmFindAndReplace := TfrmFindAndReplace.Create(Application);
    frmFindAndReplace.pagFindAndReplace.TabIndex := TabIndex;
    frmFindAndReplace.Show;
  end
  else
  begin
    frmFindAndReplace.BringToFront;
    frmFindAndReplace.pagFindAndReplace.TabIndex := TabIndex;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ShowTimings(const TabIndex: Integer = 0);
begin
  if frmTimings = NIL then
  begin
    frmTimings := TfrmTimings.Create(Application);
    frmTimings.pagTimings.TabIndex := TabIndex;
    frmTimings.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ShowTexts(const TabIndex: Integer = 0);
begin
  if frmTexts = NIL then
  begin
    frmTexts := TfrmTexts.Create(Application);
    frmTexts.pagTexts.TabIndex := TabIndex;
    frmTexts.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.ReadSubtitleValuesFromFile(const FileName: String; const FormatIndex: TUWSubtitleFormats = sfInvalid; const Mode: TShowSubtitles = ssText; const ATimes: Boolean = True);
var
  SAPI: TUWSubtitles;
  c, i: Integer;
begin
  SAPI := TUWSubtitles.Create;
  try
    if SAPI.LoadFromFile(FileName, NIL, FPS.InputFPS, FormatIndex) then
    begin
      c := Range(SAPI.Count, 0, Subtitles.Count);
      for i := 0 to c-1 do
      begin
        if ATimes then
        begin
          SetSubtitleTime(i, SAPI[i].InitialTime, tedInitial.Tag, False, False);
          SetSubtitleTime(i, SAPI[i].FinalTime, tedFinal.Tag, False, False);
        end
        else
        begin
          SetSubtitleText(i, SAPI[i].Text, Mode = ssText, False, False);
        end;
      end;
      UpdateCPSAndTexts;
      VST.Invalidate;
      IncrementUndoGroup;
    end;
  finally
    SAPI.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ WAVE }

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItem(Sender: TObject;
  const Index: Integer; const SubtitleItem: TUWSubtitleItem;
  const IsDynamic: Boolean);
begin
  VSTSelectNode(Index, True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItemChange(Sender: TObject);
begin
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectedSubtitleItemChanged(Sender: TObject;
  const Index: Integer; const OldInitialTime, OldFinalTime: Integer;
  const NeedSort: Boolean);
var
  Item : TUWSubtitleItem;
begin
  Item := Subtitles[Index];
  Item.InitialTime := OldInitialTime;
  Item.FinalTime   := OldFinalTime;

  Undo.AddUndo(utSubtitleChange, Index, Item, LastUndoGroup);
  IncrementUndoGroup;
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.WAVESelectionChange(Sender: TObject);
begin
  if WAVE.SelectionIsEmpty then
    stbStatus.Panels[1].Text := ''
  else
    stbStatus.Panels[1].Text := Format(Strings.Selection,
      [TimeToString(WAVE.Selection.InitialTime, DefTimeFormat, FPS.FPS, True),
      TimeToString(WAVE.Selection.FinalTime, DefTimeFormat, FPS.FPS, True)]);
end;

// -----------------------------------------------------------------------------

{ VLC }

// -----------------------------------------------------------------------------

procedure TfrmMain.VLCClick(Sender: TObject);
begin
  actMediaPlay.Execute;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VLCMediaPlayerLengthChanged(Sender: TObject; time: Int64);
begin
  sbrSeek.Max := time;
  ttTimes.Duration := MSecsToRefTime(time);
  lblMediaTime.Caption := TimeToString(time, DefTimeFormat);
  EnableVideoControls(time > 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VLCMediaPlayerTimeChanged(Sender: TObject; time: Int64);
begin
  if not sbrSeek.MouseIsDown then sbrSeek.Position := time;
  VLC.MarqueeShowText(GetSubtitleTextAtTime(time),
    libvlc_position_bottom, libvlc_video_marquee_color_Yellow, Options.VLCFontSize, 222);
  lblMediaTime.Caption := TimeToString(time, DefTimeFormat) + ' / ' + TimeToString(VLC.GetVideoLenInMs(), DefTimeFormat);

  // -- WaveDisplay
  if WAVE.IsPeakDataLoaded then
  begin
    WAVE.SetPlayCursorMS(time);
    if (MediaPlayMode = TMediaPlayMode.mpmSelection) and (not WAVE.SelectionIsEmpty)
      and (time >= WAVE.Selection.FinalTime) then
      begin
        VLC.Pause();
        WAVE.SetPlayCursorMS(WAVE.Selection.FinalTime);
        //VLC.SetVideoPosInMs(Int64(WAVE.Selection.FinalTime));
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.VLCMediaPlayerTitleChanged(Sender: TObject; title: Integer);
begin
  ///
end;

// -----------------------------------------------------------------------------

{ Actions }

// -----------------------------------------------------------------------------

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actModeTimeExecute(Sender: TObject);
begin
  if actModeTime.Checked then
    actModeFrames.Execute
  else
  begin
    actModeTime.Checked := True;
    WorkMode := wmTime;
    SetTimeEditMode(temTime);
    VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actModeFramesExecute(Sender: TObject);
begin
  if actModeFrames.Checked then
    actModeTime.Execute
  else
  begin
    actModeFrames.Checked := True;
    WorkMode := wmFrames;
    SetTimeEditMode(temFrames);
    VST.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTranslatorModeExecute(Sender: TObject);
begin
  SetTranslatorMode(actTranslatorMode.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  Undo.Undo;
  if WAVE.IsPeakDataLoaded then WAVE.ClearSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actRedoExecute(Sender: TObject);
begin
  Undo.Redo;
  if WAVE.IsPeakDataLoaded then WAVE.ClearSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInsertSubtitleExecute(Sender: TObject);
begin
  VSTInsertSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actInsertSubtitleBeforeExecute(Sender: TObject);
begin
  VSTInsertSubtitles(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDeleteSubtitleExecute(Sender: TObject);
begin
  VSTDeleteSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMarkSubtitleExecute(Sender: TObject);
begin
  VSTMarkSubtitles(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actUnMarkSubtitleExecute(Sender: TObject);
begin
  VSTMarkSubtitles(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSelectAllExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VST.SelectAll(False)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then Memo.SelectAll;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSubtitleDblClickExecute(Sender: TObject);
begin
  VSTDblClick(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actVideoPreviewExecute(Sender: TObject);
begin
  VideoPreview := actVideoPreview.Checked;
  sptVideo.Visible := VideoPreview;
  lyoVideo.Visible := VideoPreview;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAudioPreviewExecute(Sender: TObject);
begin
  AudioPreview := actAudioPreview.Checked;
  WAVE.Visible := AudioPreview;
  tlbWaveControls.Visible := AudioPreview;
  if AudioPreview then
  begin
    lyoVideoControls.Height := lyoVideoControls.Height + tlbWaveControls.Height;  // 76
    if not VideoPreview then actVideoPreview.Execute;
  end
  else
    lyoVideoControls.Height := lyoVideoControls.Height - tlbWaveControls.Height; // 54;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSimpleModeExecute(Sender: TObject);
begin
  tlbMain.Visible  := not actSimpleMode.Checked;
  tlbExtra.Visible := tlbMain.Visible;
  lyoEditorTopPanel.Visible  := tlbMain.Visible;
  lyoEditorLeftPanel.Visible := tlbMain.Visible;
  FormResize(Sender);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindExecute(Sender: TObject);
begin
  ShowFindAndReplace;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFindNextExecute(Sender: TObject);
begin
  with Options do
    if TextToFind <> '' then VSTFind(TextToFind, False, False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actQuickFindExecute(Sender: TObject);
begin
  if not cboFind.Focused and tlbMain.Visible then cboFind.SetFocus;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReplaceExecute(Sender: TObject);
begin
  ShowFindAndReplace(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actGoToExecute(Sender: TObject);
begin
  ShowFindAndReplace(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToNoneExecute(Sender: TObject);
begin
  SetAlignTo(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToLeftExecute(Sender: TObject);
begin
  SetAlignTo(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToCenterExecute(Sender: TObject);
begin
  SetAlignTo(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAlignToRightExecute(Sender: TObject);
begin
  SetAlignTo(3);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontBoldExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontBold)
  else
    SetTextTag(swt_Bold);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontItalicExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontItalic)
  else
    SetTextTag(swt_Italic);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontUnderlineExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontUnderline)
  else
    SetTextTag(swt_Underline);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontStrikeoutExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontStrikeout)
  else
    SetTextTag(swt_Strikeout);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontClearExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontClear)
  else
    with GetMemoFocused do Text := RemoveSWTags(Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actFontColorExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyFontColor)
  else
    SetTextTagColor(Format('%s%s%s', [IntToHex(GetRValue(LastSubtitle.Color), 2),
      IntToHex(GetGValue(LastSubtitle.Color), 2),
      IntToHex(GetBValue(LastSubtitle.Color), 2)]));
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCopyExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VSTCopySubtitlesToClipboard
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.CopyToClipboard
    else
      ComboCopyToClipboard;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCutExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VSTCopySubtitlesToClipboard(True)
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.CutToClipboard
    else
      ComboCopyToClipboard(True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPasteExecute(Sender: TObject);
var
  Memo: TMemo;
begin
  if VST.Focused then
    VSTPasteSubtitlesFromClipboard
  else
  begin
    Memo := GetMemoFocused;
    if Memo <> NIL then
      Memo.PasteFromClipboard;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actPreviousSubtitleExecute(Sender: TObject);
begin
  SelectSubtitleAndFocusMemo(False);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actNextSubtitleExecute(Sender: TObject);
begin
  SelectSubtitleAndFocusMemo(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actNewSubtitleExecute(Sender: TObject);
begin
  if CloseSubtitle then EnableWorkArea;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCloseSubtitleExecute(Sender: TObject);
begin
  CloseSubtitle;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadSubtitleExecute(Sender: TObject);
var
  OD : TOpenDialog;
begin
  OD := TOpenDialog.Create(Self);
  try
    OD.Filter := Subtitles.FillDialogFilter(Strings.AllSupportedFiles);
    if OD.Execute then
    begin
      LoadSubtitle(OD.FileName, TUWSubtitleFormats(OD.FilterIndex-1));
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSaveSubtitleAsExecute(Sender: TObject);
var
  SD : TSaveDialog;
begin
  SD := TSaveDialog.Create(Self);
  try
    SD.Filter := Subtitles.FillDialogFilter('');
    SD.FileName := ChangeFileExt(ExtractFileName(SubtitleFile.Text.FileName), '');
    if SD.Execute then
    begin
      SaveSubtitle(SD.FileName, TUWSubtitleFormats(SD.FilterIndex));
    end;
  finally
    SD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayExecute(Sender: TObject);
begin
  VLCPlay;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlaySelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayFromSelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmFromSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayBeforeSelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmBeforeSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaPlayAfterSelectionExecute(Sender: TObject);
begin
  VLCPlay(mpmAfterSelection);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaStopExecute(Sender: TObject);
begin
  VLC.SetVideoPosInMs(0);
  VLC.Pause(); //VLC.Stop();
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomInExecute(Sender: TObject);
begin
  Wave.ZoomIn;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomOutExecute(Sender: TObject);
begin
  Wave.ZoomOut;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaZoomSelectionExecute(Sender: TObject);
begin
  Wave.ZoomSelection;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaAutoScrollExecute(Sender: TObject);
begin
  ///
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaRewindExecute(Sender: TObject);
begin
  VLCSeekTo(False, 100);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaForwardExecute(Sender: TObject);
begin
  VLCSeekTo(True, 100);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaNextFrameExecute(Sender: TObject);
begin
  VLC.NextFrame();
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaRewindExExecute(Sender: TObject);
begin
  VLCSeekTo(False, 5000);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaForwardExExecute(Sender: TObject);
begin
  VLCSeekTo(True, 5000);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaChangePlayRateExecute(Sender: TObject);
begin
  VLCAlterPlayRate(actMediaChangePlayRate.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSetInitialTimeExecute(Sender: TObject);
begin
  if not (VLC.GetVideoLenInMs() > 0) or (VSTFocusedNode < 0) then Exit;
  VSTDoLoop(@ApplySetTimeInitialFromVLC); //SetSubtitleTime(VSTFocusedNode, VLC.GetVideoPosInMs(), 0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaSetFinalTimeExecute(Sender: TObject);
begin
  if not (VLC.GetVideoLenInMs() > 0) or (VSTFocusedNode < 0) then Exit;
  VSTDoLoop(@ApplySetTimeFinalFromVLC); //SetSubtitleTime(VSTFocusedNode, VLC.GetVideoPosInMs(), 1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaStartSubtitleExecute(Sender: TObject);
begin
  if not (VLC.GetVideoLenInMs() > 0) then Exit;
  LastSubtitle.InitialTime := VLC.GetVideoPosInMs();
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaEndSubtitleExecute(Sender: TObject);
var
  i, l: Integer;
begin
  if not (VLC.GetVideoLenInMs() > 0) then Exit;

  if VSTFocusedNode >= 0 then
    i := VSTFocusedNode + 1
  else
    i := -1;

 l := VLC.GetVideoPosInMs();
 if l >= LastSubtitle.InitialTime then
   InsertSubtitle(i, LastSubtitle.InitialTime, l, '', '');
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actMediaAddSubtitleExecute(Sender: TObject);
var
  Item: TUWSubtitleItem;
begin
  if WAVE.IsPeakDataLoaded then
  begin
    if WAVE.SelectionIsEmpty and (WAVE.CursorPosMS > 0) then
    begin
      ClearSubtitleItem(Item);
      Item.InitialTime := WAVE.CursorPosMS;
      Item.FinalTime   := Item.InitialTime + Options.NewSubtitleMS;
      WAVE.SelectSubtitle(InsertSubtitle(Subtitles.FindInsertPos(Item), Item), True, False);
    end
    else if not WAVE.SelectionIsEmpty and WAVE.IsOnlySelection then
    begin
      WAVE.SelectSubtitle(InsertSubtitle(Subtitles.FindInsertPos(WAVE.Selection), WAVE.Selection), True, False);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftToPreviousExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftToPrevious, dlSelected, True);
  UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftToNextExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftToNext, dlSelected, True);
  UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutomaticDurationExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyAutomaticDuration, dlSelected, True);
  UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDefaultPauseExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyDefaultPause, dlSelected, True);
  UpdateValues;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actExecuteExtensionExecute(Sender: TObject);
begin
  ///
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSpellCheckExecute(Sender: TObject);
begin
  if frmSpellCheck = NIL then
  begin
    frmSpellCheck := TfrmSpellCheck.Create(Application);
    frmSpellCheck.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  if frmAbout = NIL then
  begin
    frmAbout := TfrmAbout.Create(Application);
    frmAbout.ShowModal;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewNumberExecute(Sender: TObject);
begin
  VSTShowColumn(0, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewTimesExecute(Sender: TObject);
begin
  VSTShowColumn(1, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewDurationExecute(Sender: TObject);
begin
  VSTShowColumn(2, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewStyleExecute(Sender: TObject);
begin
  VSTShowColumn(3, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewCharsExecute(Sender: TObject);
begin
  VSTShowColumn(6, (Sender as TAction).Checked);
  VSTResize(VST);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actViewToolbarAdditionalExecute(Sender: TObject);
begin
  tlbExtra.Visible := (Sender as TAction).Checked;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDurationLimitsExecute(Sender: TObject);
begin
  ShowTimings(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutomaticDurationsExecute(Sender: TObject);
begin
  ShowTimings(1);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDurationExpanderReducerExecute(Sender: TObject);
begin
  ShowTimings(2);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actDelayExecute(Sender: TObject);
begin
  ShowTimings(3);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actConvertCaseExecute(Sender: TObject);
begin
  ShowTexts(0);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReadTimingsFromFileExecute(Sender: TObject);
var
  OD : TOpenDialog;
begin
  OD := TOpenDialog.Create(Self);
  try
    OD.Filter := Subtitles.FillDialogFilter(Strings.AllSupportedFiles);
    if OD.Execute then
    begin
      ReadSubtitleValuesFromFile(OD.FileName, TUWSubtitleFormats(OD.FilterIndex-1));
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actReadTextsFromFileExecute(Sender: TObject);
var
  OD : TOpenDialog;
begin
    OD := TOpenDialog.Create(Self);
    try
      OD.Filter := Subtitles.FillDialogFilter(Strings.AllSupportedFiles);
      if OD.Execute then
      begin
        if not TranslatorMode then
          ReadSubtitleValuesFromFile(OD.FileName, TUWSubtitleFormats(OD.FilterIndex-1), ssText, False)
        else
          ReadSubtitleValuesFromFile(OD.FileName, TUWSubtitleFormats(OD.FilterIndex-1), ssTranslation, False);
      end;
    finally
      OD.Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimeMoreExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftTimeMore);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actShiftTimeLessExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyShiftTimeLess);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTextAutobreakExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyAutobreak)
  else
    with GetMemoFocused do Text := AutoBreakSubtitle(Text, Options.MaxLineLength);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTextUnbreakExecute(Sender: TObject);
begin
  if GetMemoFocused = NIL then
    VSTDoLoop(@ApplyUnbreak)
  else
    with GetMemoFocused do Text := UnbreakSubtitles(Text);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTextSetMaxLineLengthExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplySetMaximumLineLength);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTextReverseExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyReverseText);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actTextFixPunctuationExecute(Sender: TObject);
begin
  VSTDoLoop(@ApplyFixPunctuation);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutoDivideSubtitlesExecute(Sender: TObject);
begin
  VSTDivideSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actAutoCombineSubtitlesExecute(Sender: TObject);
begin
  VSTCombineSubtitles;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actSortExecute(Sender: TObject);
begin
  Subtitles.Sort;
  UpdateValues(True);
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actLoadVideoExecute(Sender: TObject);
var
  OD : TOpenDialog;
  i  : Integer;
  s  : String;
begin
  OD := TOpenDialog.Create(Self);
  try
    s := Strings.VideoFiles + '|';
    for i := 0 to High(TVideoExts) do s := s + '*' + TVideoExts[i] + ';';
    OD.Filter := s;
    if OD.Execute then
    begin
      OpenVideo(OD.FileName);
    end;
  finally
    OD.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmMain.actCloseVideoExecute(Sender: TObject);
begin
  VLC.Play('');
  sbrSeek.Max          := 0;
  ttTimes.Duration     := 1;
  lblMediaTime.Caption := TimeToString(0, DefTimeFormat);
  EnableVideoControls(False);
end;

// -----------------------------------------------------------------------------

end.

