{*
 *  URUWorks Subtitle API
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

unit UWSubtitleAPI.ExtraInfo;

// -----------------------------------------------------------------------------

interface

uses UWSubtitleAPI.Formats.EBU.Types, UWSubtitleAPI.Formats.Cavena890.Types;

// -----------------------------------------------------------------------------

type

  { TExtraInfoType }

  TUWSubtitleExtraInfoType =
  (
    eiNone,
    eiCavena890,
    eiEBU,
    eiMicroDVD,
    eiSSA,
    eiSubRip,
    eiWebVTT
  );

  { Advanced Subtitles }

  PXAS_Info = ^TXAS_Info;
  TXAS_Info = record
    Language  : AnsiString;
    FontName  : AnsiString;
    FontSize  : Byte;
    FontColor : Cardinal;
    X         : Byte;
    Y         : Byte;
    W         : Byte;
    H         : Byte;
    Alignment : Byte;
  end;

  { Cavena 890 }

  PCavena890_Info = ^TCavena890_Info;
  TCavena890_Info = THeaderBlock;

  { DVDSubtitle Info }

  PDVDSubtitle_Info = ^TDVDSubtitle_Info;
  TDVDSubtitle_Info = record
    Assigned : Boolean;
    DiskId   : String;
    DVDTitle : String;
    Language : String;
    Author   : String;
    Web      : String;
    Info     : String;
    License  : String;
  end;

  { EBU }

  PEBU_Info = ^TEBU_Info;
  TEBU_Info = record
    DiskFormatCode            : Byte;       // DFC (0= STL25.01 or  1= STL30.01)
    CodePageNumber            : AnsiString; // CPN (i.e. 850)
    DisplayStandardCode       : Byte;       // DSC
    CharCodeTableNumber       : AnsiString; // CCT
    LanguageCode              : AnsiString; // LC
    CountryOrigin             : AnsiString; // CO
    MaxNumberDisplayableChars : AnsiString; // MNC
    MaxNumberDisplayableRows  : AnsiString; // MNR
  end;

  PEBU_ExtraInfo = ^TEBU_ExtraInfo;
  TEBU_ExtraInfo = record
    CumulativeStatus  : Byte; // CS_Not, CS_First, CS_Intermediate, CS_Last
    VerticalPosition  : Byte; //
    JustificationCode : Byte; // JC_Unchanged, JC_Left, JC_Center, JC_Center, JC_Right
    CommentFlag       : Byte; // CF_TFHaveData, CF_TFHaveComments
  end;

  { MicroDVD }

  PMicroDVD_ExtraInfo = ^TMicroDVD_ExtraInfo;
  TMicroDVD_ExtraInfo = record
    X: Integer;
    Y: Integer;
  end;

  { SSA }

  TSSAEventType  = (etDialogue, etComment, etPicture, etSound, etCommand);
  PSSA_ExtraInfo = ^TSSA_ExtraInfo;
  TSSA_ExtraInfo = record
    EventType : TSSAEventType; // Event (most times "Dialogue)         (Default = "Dialogue")
    Marked    : Boolean;       // 0 = False; 1 = True                  (Default = "0")
    Style     : AnsiString;    // Style name                           (Default = "Default")
    Name      : AnsiString;    // Name of the person who is speaking   (Default = "NTP")
    MarginL   : Integer;       // Left margin (4 numbers)              (Default = "0000")
    MarginR   : Integer;       // Right margin (4 numbers)             (Default = "0000")
    MarginV   : Integer;       // Vertical margin (4 numbers)          (Default = "0000")
    Effect    : AnsiString;    // Effect                               (Default = "!Effect")
  end;

  { SubRip }

  PSubRip_ExtraInfo = ^TSubRip_ExtraInfo;
  TSubRip_ExtraInfo = record
    X1: Integer; // Left
    X2: Integer; // Right
    Y1: Integer; // Top
    Y2: Integer; // Bottom
  end;

  { WebVTT }

  PWebVTT_ExtraInfo = ^TWebVTT_ExtraInfo;
  TWebVTT_ExtraInfo = record
    VerticalText : Integer;    // D:vertical (vertical growing left)
                               // D:vertical-lr (vertical growing right)
    LinePos      : Integer;    // L:[a number]%, where [a number] is a positive integer.
                               // L:[a number], where [a number] is a positive or negative integer.
    TextPos      : Integer;    // T:[a number]%, where [a number] is a positive integer.
    TextSize     : Integer;    // S:[a number]%, where [a number] is a positive integer.
    TextAlign    : AnsiString; // A:start or A:middle or A:end
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

end.
