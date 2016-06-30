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

unit UTypes;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Graphics, Types, UWSubtitleAPI, UWSubtitleAPI.Formats, UErrors,
  UUndo, UWFiles.MRU, UWSpellCheck.Hunspell;

const

  ProgramName       = 'Subtitle Workshop';
  ProgramWebsite    = 'http://uruworks.net';
  ProgramVer        = 07001; // First 4 digits / 1000 = version, last digit: 0 = Final, >0 = Beta

  DefTimeFormat     = 'hh:mm:ss.zzz';
  DefDurationFormat = 'mm:ss.zzz';
  DefFPS            = 23.976;
  DefFPSList        : array[0..7] of Single = (15, 20, 23.976, 23.978, 24, 25, 29.997, 30);

  swt_StartTag      = '{';
  swt_EndTag        = '}';
  swt_Bold          = 'B';
  swt_Italic        = 'I';
  swt_Underline     = 'U';
  swt_Strikeout     = 'S';
  swt_Color         = 'C';

  swt_Sing          = '♪';

  TVideoExts : array[0..19] of String =
  (
    '.avi', '.mp4', '.mpg', '.mpeg', '.mkv', '.webm', '.flv', '.ogv', '.ogg',
    '.mov', '.qt', '.wmv', '.rm', '.rmvb', '.asf', '.m4v', '.m4p', '.mpv',
    '.mpe', '.nsv'
  );

  TAudioExts : array[0..1] of String =
  (
    '.wav', '.uwp'
  );

type

  TWorkMode = (wmTime, wmFrames);

  TShowSubtitles = (ssText, ssTranslation);

  TDblClickMode = (dcmFocusTextBox, dcmGoSubtitleTime, dcmGoNBeforeSubtitleTime);

  TFPS = packed record // used for conversions
    InputFPS : Single;
    FPS      : Single;
  end;

  TColors = packed record // used for mark errors
    Overlapping   : TColor;
    BadValues     : TColor;
    TimeTooShort  : TColor;
    TimeTooLong   : TColor;
    PauseTooShort : TColor;
    Untranslated  : TColor;
  end;

  TOptions = packed record
    Language               : String;
    AutoCheckErrors        : Boolean;
    ErrOptions             : TSubtitleErrorTypeSet;
    ErrCfg                 : TErrCfg; // UErrors
    Colors                 : TColors;
    DrawTags               : Boolean;
    NewSubtitleMS          : Cardinal;
    DefSubtitlePauseMS     : Cardinal;
    DefChangePlayRate      : Integer;
    ShiftTime              : Integer;
    MaxLineLength          : Integer;
    DotsOnSplit            : Boolean;
    TextToFind             : String;
    ShowSubtitles          : TShowSubtitles;
    DblClickMode           : TDblClickMode;
    DblClickModeValue      : Byte;
    ShiftDblClickMode      : TDblClickMode;
    ShiftDblClickModeValue : Byte;
    VLCFontSize            : Integer;
    HunspellLang           : String;
    ShowWelcomeAtStartup   : Boolean;
  end;

  TLangStrings = packed record
    Text,
    TextChars,
    TranslationChars,
    Translation,
    LineSelected,
    AllSupportedFiles,
    VideoFiles,
    Selection,
    SaveDialog: String;
  end;

  TErrorStrings = packed record
    Marked,
    BadValues,
    TimeTooLong,
    TimeTooShort,
    PauseTooShort,
    MaxCPS,
    Overlapping,
    FixTags,
    Empty,
    UnnecessarySpaces,
    UnnecessaryDots,
    RepeatedChars,
    ProhibitedChars,
    HearingImpaired,
    BreakLongLines,
    RepeatedSubtitle,
    OCR,
    ExecuteScript,
    CompileScript: String;
  end;

  TLastSubtitle  = packed record
    Selected    : Integer;
    InitialTime : Cardinal;
    Color       : TColor;
  end;

  TSubtitleFileInfo = packed record
    FileName : String;
    Format   : TUWSubtitleFormats;
    Changed  : Boolean;
  end;

  TSubtitleFiles  = packed record
    Text,
    Translation : TSubtitleFileInfo;
  end;

  { TMediaPlayMode }

  TMediaPlayMode = (mpmAll, mpmSelection, mpmFromSelection, mpmBeforeSelection, mpmAfterSelection);

  { VST Proc }

  TUWSubtitleDoLoopProc = procedure(const Item: PUWSubtitleItem; const Index: Integer);
  TUWSubtitleDoLoopSelection = (dlAll, dlSelected, dlCurrentToLast, dlMarked);

var

  Subtitles      : TUWSubtitles;       // SubtitleAPI
  WorkMode       : TWorkMode = wmTime; //
  TranslatorMode : Boolean = True;
  VideoPreview   : Boolean = True;
  AudioPreview   : Boolean = True;
  TextSize       : TSize;              // used in VST DrawText
  FormatSettings : TFormatSettings;
  FPS            : TFPS;
  Options        : TOptions;
  Strings        : TLangStrings;
  ErrorStrings   : TErrorStrings;
  LastSubtitle   : TLastSubtitle;
  SubtitleFile   : TSubtitleFiles;
  MediaPlayMode  : TMediaPlayMode = mpmAll;
  Undo           : TUndo;
  LastUndoGroup  : Byte = 0;
  MRU            : TUWMRU;
  Hunspell       : TUWHunspell;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

end.

