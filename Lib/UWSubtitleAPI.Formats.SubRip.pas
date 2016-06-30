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

unit UWSubtitleAPI.Formats.SubRip;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWSubRip }

  TUWSubRip = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags;

{
First line is the subtitle index, it's just a sequential number.
Second line is the start and stop time, it can optionally include subtitle coordinates in pixels as a bounding box (X1:left X2:right Y1:top Y2:bottom).
The third line is the subtitle text (one or more lines, with optional tags)
Each subtitle is separated by a blank line.

The start and stop time have this format: HH:MM:SS,MIL (hours, minutes, seconds, milliseconds)

The available official tags are:

<b></b> : bold
<i></i> : italic
<u></u> : underline
<font color=�#rrggbb�></font> : text color using 3 color components, red, green and blue.
}

// -----------------------------------------------------------------------------

function TUWSubRip.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWSubRip.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfSubRip;
end;

// -----------------------------------------------------------------------------

function TUWSubRip.Extension: String;
begin
  Result := '*.srt';
end;

// -----------------------------------------------------------------------------

function TUWSubRip.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWSubRip.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags and coords
end;

// -----------------------------------------------------------------------------

function TUWSubRip.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos(' --> ', SubtitleFile[Row]) > 0) and
    TimeInFormat(Copy(ReplaceString(SubtitleFile[Row], ' ', ''), 1, 12), 'hh:mm:ss,zzz') then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWSubRip.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i           : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text, s     : String;
  ExtraInfo   : PSubRip_ExtraInfo;
  X1, X2,
  Y1, Y2      : Integer;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if Pos(' --> ', SubtitleFile[i]) > 0 then
      begin
        s := ReplaceString(SubtitleFile[i], ' ', '');

        InitialTime := StringToTime(Copy(s, 1, 12));
        FinalTime   := StringToTime(Copy(s, 16, 12));

        X1 := Pos('X1', SubtitleFile[i]); X2 := Pos('X2', SubtitleFile[i]);
        Y1 := Pos('Y1', SubtitleFile[i]); Y2 := Pos('Y2', SubtitleFile[i]);
        if (X1 > 0) and (X2 > 0) and (Y1 > 0) and (Y2 > 0) then
        begin
          New(ExtraInfo);  // X1:left X2:right Y1:top Y2:bottom
          ExtraInfo^.X1 := StrToIntDef(Copy(SubtitleFile[i], X1+3, ((X2-1)-(X1+3))), 0);
          ExtraInfo^.X2 := StrToIntDef(Copy(SubtitleFile[i], X2+3, ((Y1-1)-(X2+3))), 0);
          ExtraInfo^.Y1 := StrToIntDef(Copy(SubtitleFile[i], Y1+3, ((Y2-1)-(Y1+3))), 0);
          ExtraInfo^.Y2 := StrToIntDef(Copy(SubtitleFile[i], Y2+3, Length(SubtitleFile[i])-(Y2+2)), 0);
        end
        else
          ExtraInfo := NIL;

        if (InitialTime > -1) and (FinalTime > -1) then
        begin
          Inc(i);
          Text := '';
          while (i < SubtitleFile.Count) and (Pos(' --> ', SubtitleFile[i]) = 0) and
            not IsInteger(SubtitleFile[i]) do
          begin
            if Text <> '' then
              Text := Text + sLineBreak + SubtitleFile[i]
            else
              Text := SubtitleFile[i];

            Inc(i);
          end;

          Text := HTMLTagsToSW(Text);
          Subtitles.Add(InitialTime, FinalTime, Text, '', ExtraInfo, False);
        end;
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then
    begin
      Subtitles.ExtraInfoType := eiSubRip;
      Result := True;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TUWSubRip.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  i       : Integer;
  Count   : Integer;
  Text    : String;
  XY      : String;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    Count := 1;
    for i := FromItem to ToItem do
    begin
      SubFile.Add(IntToStr(Count), False);
      Inc(Count);
      if Subtitles.ExtraInfo[i] <> NIL then
        with PSubRip_ExtraInfo(Subtitles.ExtraInfo[i])^ do
        begin
          if (X1 <> 0) or (X2 <> 0) or (Y1 <> 0) or (Y2 <> 0) then
            XY := SysUtils.Format(' X1:%d X2:%d Y1:%d Y2:%d', [X1, X2, Y1, Y2])
          else
            XY := '';
        end
      else
        XY := '';

      SubFile.Add(TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss,zzz') + ' --> ' + TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss,zzz') + XY, False);
      Text := SWTagsToHTML(Subtitles.Text[i]);
      SubFile.Add(Text, False);
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

function TUWSubRip.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
