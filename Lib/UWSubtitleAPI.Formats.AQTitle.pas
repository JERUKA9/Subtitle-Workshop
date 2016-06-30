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

unit UWSubtitleAPI.Formats.AQTitle;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, StrUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWAQTitle }

  TUWAQTitle = class(TUWSubtitleCustomFormat)
  public
    function Name: String; override;
    function Format: TUWSubtitleFormats; override;
    function Extension: String; override;
    function IsTimeBased: Boolean; override;
    function HasStyleSupport: Boolean; override;
    function IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean; override;
    function LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean; override;
    function SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean; override;
    function ToText(const Subtitles: TUWSubtitles): String; override;
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function TUWAQTitle.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAQTitle;
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.Extension: String;
begin
  Result := '*.aqt';
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.IsTimeBased: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.HasStyleSupport: Boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (IsInteger(Copy(SubtitleFile[Row], 6, Length(SubtitleFile[Row])))) and
     (Pos('-->> ', SubtitleFile[Row]) = 1) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i, c        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if (Pos('-->> ', SubtitleFile[i]) = 1) and
         (IsInteger(Copy(SubtitleFile[i], 6, Length(SubtitleFile[i])))) then
      begin
        c    := 1;
        Text := '';
        InitialTime := FramesToTime(StrToIntDef(Copy(SubtitleFile[i], 6, 6), -1), FPS);

        while (i+c < (SubtitleFile.Count-1)) and (Copy(SubtitleFile[i+c], 1, 5) <> '-->> ') do
        begin
          If Text <> '' then
            Text := Text + sLineBreak + SubtitleFile[i+c]
          else
            Text := SubtitleFile[i+c];
          Inc(c);
        end;
        If (IsInteger(Copy(SubtitleFile[i+c], 6, 2))) then
          FinalTime := FramesToTime(StrToIntDef(Copy(SubtitleFile[i+c], 6, 6), -1), FPS)
        else
          FinalTime := InitialTime + 2000;

        Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  i       : Integer;
  Text    : String;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    for i := FromItem to ToItem do
    begin
      SubFile.Add('-->> ' + AddChar('0', IntToStr(TimeToFrames(Subtitles[i].InitialTime, FPS)), 6), False);

      if StringCount(#13#10, Subtitles[i].Text) = 0      then Text := Subtitles[i].Text + #13#10
      else if StringCount(#13#10, Subtitles[i].Text) = 1 then Text := Subtitles[i].Text
      else if StringCount(#13#10, Subtitles[i].Text) > 1 then Text := Copy(Subtitles[i].Text, 0, Pos(#13#10, Subtitles[i].Text) + 2) + ReplaceEnters(Copy(Subtitles[i].Text, Pos(#13#10, Subtitles[i].Text) + 2, Length(Subtitles[i].Text)), ' ');

      SubFile.Add(Text, False);
      SubFile.Add('-->> ' + AddChar('0', IntToStr(TimeToFrames(Subtitles[i].FinalTime, FPS)), 6), False);
      SubFile.Add('', False);
      SubFile.Add('', False);
    end;

    try
      SubFile.SaveToFile(FileName, Encoding);
      Result := True;
    except
    end;
  finally
    SubFile.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAQTitle.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
