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

unit UExtensions;

// -----------------------------------------------------------------------------

{$mode objfpc}{$H+}

interface

uses
  SysUtils, uPSComponent, uPSCompiler, uPSRuntime;

procedure psCompImport(Sender: TObject; x: TPSPascalCompiler);
procedure psExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
procedure psCompile(Sender: TPSScript);

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain, Dialogs,
  uPSC_Buttons, uPSC_ComObj, uPSC_Controls, uPSC_Classes, uPSC_DateUtils,
  uPSC_Std, uPSC_StdCtrls, uPSC_Forms, uPSC_ExtCtrls, uPSC_Graphics, uPSC_Menus,
  uPSR_Buttons, uPSR_ComObj, uPSR_Controls, uPSR_Classes, uPSR_DateUtils,
  uPSR_Std, uPSR_StdCtrls, uPSR_Forms, uPSR_ExtCtrls, uPSR_Graphics, uPSR_Menus;

// -----------------------------------------------------------------------------

function psGetSWVersion: Integer;
begin
  Result := ProgramVer;
end;

// -----------------------------------------------------------------------------

function psGetSubCount: Integer;
begin
  Result := Subtitles.Count;
end;

// -----------------------------------------------------------------------------

function psIsSelSub(const Index: Integer): Boolean;
begin
  Result := frmMain.VST.Selected[VSTGetNodeAtIndex(Index)];
end;

// -----------------------------------------------------------------------------

function psGetSubText(const Index: Integer): String;
begin
  Result := Subtitles.Text[Index];
end;

// -----------------------------------------------------------------------------

procedure psSetSubText(const Index: Integer; const Text: String);
begin
  SetSubtitleText(Index, Text, True, False, False);
end;

// -----------------------------------------------------------------------------

function psGetSubTrans(const Index: Integer): String;
begin
  Result := Subtitles.Translation[Index];
end;

// -----------------------------------------------------------------------------

procedure psSetSubTrans(const Index: Integer; const Text: String);
begin
  SetSubtitleText(Index, Text, False, False, False);
end;

// -----------------------------------------------------------------------------

function psGetSubInitialTime(const Index: Integer): Integer;
begin
  Result := Subtitles.InitialTime[Index];
end;

// -----------------------------------------------------------------------------

procedure psSetSubInitialTime(const Index: Integer; const Time: Integer);
begin
  SetSubtitleTime(Index, Time, 0, False, False);
end;

// -----------------------------------------------------------------------------

function psGetSubFinalTime(const Index: Integer): Integer;
begin
  Result := Subtitles.FinalTime[Index];
end;

// -----------------------------------------------------------------------------

procedure psSetSubFinalTime(const Index: Integer; const Time: Integer);
begin
  SetSubtitleTime(Index, Time, 1, False, False);
end;

// -----------------------------------------------------------------------------

function psGetSubDuration(const Index: Integer): Integer;
begin
  Result := Subtitles.Duration[Index];
end;

// -----------------------------------------------------------------------------

procedure psSetSubDuration(const Index: Integer; const Time: Integer);
begin
  SetSubtitleTime(Index, Time, 2, False, False);
end;

// -----------------------------------------------------------------------------

function psGetSubPause(const Index: Integer): Integer;
begin
  Result := Subtitles.Pause[Index];
end;

// -----------------------------------------------------------------------------

procedure psSetSubPause(const Index: Integer; const Time: Integer);
begin
  SetSubtitleTime(Index, Time, 3, False, False);
end;

// -----------------------------------------------------------------------------

procedure psInsSub(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String);
begin
  InsertSubtitle(Index, InitialTime, FinalTime, Text, Translation, False, False);
end;

// -----------------------------------------------------------------------------

procedure psDelSub(const Index: Integer);
begin
  DeleteSubtitle(Index, False, False);
end;

// -----------------------------------------------------------------------------

procedure psGoToLineNumber(const Line: Integer);
begin
  VSTSelectNode(Line, True);
end;

// -----------------------------------------------------------------------------

procedure psEnableWorkArea;
begin
  frmMain.EnableWorkArea(True);
end;

// -----------------------------------------------------------------------------

function psIsOriginalLoaded: Boolean;
begin
  Result := (SubtitleFile.Text.FileName <> '') or (Subtitles.Count > 0);
end;

// -----------------------------------------------------------------------------

function psIsTranslatedLoaded: Boolean;
begin
  Result := (SubtitleFile.Translation.FileName <> '') or (Subtitles.Count > 0);
end;

// -----------------------------------------------------------------------------

function psGetOrgFileName: String;
begin
  Result := SubtitleFile.Text.FileName;
end;

// -----------------------------------------------------------------------------

function psGetTransFileName: String;
begin
  Result := SubtitleFile.Translation.FileName;
end;

// -----------------------------------------------------------------------------

procedure psRandomize;
begin
  Randomize;
end;

// -----------------------------------------------------------------------------

function psRandom(Range: Integer): Integer;
begin
  Result := Random(Range);
end;

// -----------------------------------------------------------------------------

function psMsgBox(const ACaption, AMsg: String; Buttons: array of const): Integer;
begin
  Result := QuestionDlg(ACaption, AMsg, mtCustom, Buttons, '');
end;

// -----------------------------------------------------------------------------

procedure psCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Graphics(x, True);
  SIRegister_Controls(x);
  SIRegister_StdCtrls(x);
  SIRegister_ExtCtrls(x);
  SIRegister_Forms(x);
  SIRegister_Menus(x);
  SIRegister_Buttons(x);
  SIRegister_ComObj(x);
  RegisterDateTimeLibrary_C(x);
end;

// -----------------------------------------------------------------------------

procedure psExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_StdCtrls(x);
  RIRegister_ExtCtrls(x);
  RIRegister_Forms(x);
  RIRegister_Menus(x);
  RIRegister_Buttons(x);
  RIRegister_ComObj(se);
  RegisterDateTimeLibrary_R(se);
end;

// -----------------------------------------------------------------------------

procedure psCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@psGetSubCount,        'function GetSubtitleCount: Integer;');
  Sender.AddFunction(@psIsSelSub,           'function IsSubtitleSelected(const Index: Integer): Boolean;');
  Sender.AddFunction(@psGetSubText,         'function GetSubtitleText(const Index: Integer): String;');
  Sender.AddFunction(@psSetSubText,         'procedure SetSubtitleText(const Index: Integer; const Text: String);');
  Sender.AddFunction(@psGetSubTrans,        'function GetSubtitleTrans(const Index: Integer): String;');
  Sender.AddFunction(@psSetSubTrans,        'procedure SetSubtitleTrans(const Index: Integer; const Text: String);');
  Sender.AddFunction(@psGetSubInitialTime,  'function GetSubtitleInitialTime(const Index: Integer): Integer;');
  Sender.AddFunction(@psSetSubInitialTime,  'procedure SetSubtitleInitialTime(const Index: Integer; const Time: Integer);');
  Sender.AddFunction(@psGetSubFinalTime,    'function GetSubtitleFinalTime(const Index: Integer): Integer;');
  Sender.AddFunction(@psSetSubFinalTime,    'procedure SetSubtitleFinalTime(const Index: Integer; const Time: Integer);');
  Sender.AddFunction(@psInsSub,             'procedure InsertSubtitle(const Index: Integer; const InitialTime, FinalTime: Integer; const Text, Translation: String);');
  Sender.AddFunction(@psDelSub,             'procedure DeleteSubtitle(const Index: Integer);');
  Sender.AddFunction(@psGoToLineNumber,     'procedure GoToLineNumber(const Line: Integer);');
  Sender.AddFunction(@psEnableWorkArea,     'function EnableWorkArea: Boolean;');
  Sender.AddFunction(@psIsOriginalLoaded,   'function IsOriginalLoaded: Boolean;');
  Sender.AddFunction(@psIsTranslatedLoaded, 'function IsTranslatedLoaded: Boolean;');
  Sender.AddFunction(@psGetOrgFilename,     'function GetOrgFilename(): String;');
  Sender.AddFunction(@psGetTransFilename,   'function GetTransFilename(): String;');
  Sender.AddFunction(@psRandomize,          'procedure Randomize;');
  Sender.AddFunction(@psRandom,             'function Random(Range: Integer): Integer;');
  Sender.AddFunction(@psMsgBox,             'function MsgBox(const ACaption, AMsg: String; Buttons: array of const): Integer;');
  Sender.AddFunction(@psGetSWVersion,       'function psGetSWVersion: Integer;');
//  Sender.AddFunction(@psGetAPIVersion,      'function GetAPIVersion(): String;');
//  Sender.AddFunction(@psGetOrgFilepath,     'function GetOrgFilepath(): String;');
//  Sender.AddFunction(@psGetTransFilepath,   'function GetTransFilepath(): String;');
//  Sender.AddFunction(@psGetMovieFilename,   'function GetMovieFilename(): String;');
//  Sender.AddFunction(@psGetMovieFilepath,   'function GetMovieFilepath(): String;');
//  Sender.AddFunction(@psGetVideoPosTime,    'function GetVideoPosTime(): String;');
//  Sender.AddFunction(@psGetVideoPosFrames,  'function GetVideoPosFrames(): String;');

  Sender.AddRegisteredVariable('Application', 'TApplication');
end;

// -----------------------------------------------------------------------------

end.

