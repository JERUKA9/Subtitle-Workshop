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

unit UWSubtitleAPI.Formats.AdvancedSubstationAlpha;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSystem.SysUtils, UWSubtitleAPI.Formats;

type

  { TUWAdvancedSubstationAlpha }

  TUWAdvancedSubstationAlpha = class(TUWSubtitleCustomFormat)
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

uses strUtils;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAdvancedSubstationAlpha;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.Extension: String;
begin
  Result := '*.ass';
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if (Pos('Dialogue:', SubtitleFile[Row]) > 0) and
     (Pos('Marked=', SubtitleFile[Row]) = 0) and
     (Pos('!effect', SubtitleFile[Row]) = 0) and
     (TimeInFormat(Trim(Copy(SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1, PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) - (Pos(',', SubtitleFile[Row]) + 1))), 'h:mm:ss.zz'))  and
     (TimeInFormat(Trim(Copy(SubtitleFile[Row], PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) + 1, PosEx(',', SubtitleFile[Row], PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) + 1) - (PosEx(',', SubtitleFile[Row], Pos(',', SubtitleFile[Row]) + 1) + 1))), 'h:mm:ss.zz')) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  i, a        : Integer;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  try
    i := 0;
    while i < SubtitleFile.Count do
    begin
      if Contains('Dialogue:', SubtitleFile[i]) then
      begin
        a := Pos(',', SubtitleFile[i]);
        InitialTime := StringToTime(Trim(Copy(SubtitleFile[i], a + 1, PosEx(',', SubtitleFile[i], a + 1) - (a + 1))));
        a := PosEx(',', SubtitleFile[i], a + 1);
        FinalTime   := StringToTime(Trim(Copy(SubtitleFile[i], a + 1, PosEx(',', SubtitleFile[i], a + 1) - (a + 1))));

        if (InitialTime > -1) and (FinalTime > -1) then
        begin
          Text := SubtitleFile[i];
          for a := 1 to 9 do Delete(Text, 1, Pos(',', Text));
          Text := ReplaceString(Trim(Text), '\N', #13#10);
          Text := ReplaceString(Text, '{\i1}', '{i}');
          Text := ReplaceString(Text, '{\b1}', '{b}');
          Text := ReplaceString(Text, '{\u1}', '{u}');
          Text := ReplaceString(Text, '{\i0}', '{/i}');
          Text := ReplaceString(Text, '{\b0}', '{/b}');
          Text := ReplaceString(Text, '{\u0}', '{/u}');

          Subtitles.Add(InitialTime, FinalTime, Text, '', NIL, False);
        end;
      end;
      Inc(i);
    end;
  finally
    if Subtitles.Count > 0 then Result := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubstationAlpha.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  SubFile : TUWStringList;
  i       : Integer;
  Text    : String;
begin
  Result  := False;
  SubFile := TUWStringList.Create;
  try
    SubFile.Add('[Script Info]', False);
    SubFile.Add('ScriptType: v4.00+', False);
//      SubFile.Add('Collisions: ' + ASSAttributes.Collisions, False);
//      SubFile.Add('PlayResX: ' + IntToStr(ASSAttributes.PlayResX), False);
//      SubFile.Add('PlayResY: ' + IntToStr(ASSAttributes.PlayResY), False);
//      SubFile.Add('Timer: ' + ASSAttributes.Timer, False);
    SubFile.Add('', False);
    SubFile.Add('[V4+ Styles]', False);
//    SubFile.Add(Format, False);
//      SubFile.Add(GetASSStyleStr);
    SubFile.Add('', False);
    SubFile.Add('[Events]', False);
    SubFile.Add('Format: Layer, Start, End, Style, Actor, MarginL, MarginR, MarginV, Effect, Text', False);

    for i := FromItem to ToItem do
    begin
      Text := Subtitles[i].Text;
      Text := ReplaceString(Trim(Text), '\N', #13#10);
      Text := ReplaceString(Text, '{i}', '{\i0}');
      Text := ReplaceString(Text, '{b}', '{\b0}');
      Text := ReplaceString(Text, '{u}', '{\u0}');
      Text := ReplaceString(Text, '{/i}', '{\i1}');
      Text := ReplaceString(Text, '{/b}', '{\b1}');
      Text := ReplaceString(Text, '{/u}', '{\u1}');

      SubFile.Add('Dialogue: 0,' + TimeToString(Subtitles[i].InitialTime, 'h:mm:ss.zz') + ',' + TimeToString(Subtitles[i].FinalTime, 'h:mm:ss.zz') + ',Default,,0000,0000,0000,,'+ ReplaceEnters(Text,'\N'), False);
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

function TUWAdvancedSubstationAlpha.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
