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

unit UWSubtitleAPI.TimeCode;

// -----------------------------------------------------------------------------

{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  SysUtils;

type

  { UWTimeCode }

  TUWTimeCode       = Integer;
  TUWTimeCodeHelper = record helper for TUWTimeCode
    procedure EncodeTime(const Hour, Min, Secs, MSecs: Word); // to MS
    procedure DecodeTime(out Hour, Min, Secs, MSecs: Word);   // from MS
    function ToFrames(const FPS: Single): Cardinal;
  end;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.TimeUtils;

// -----------------------------------------------------------------------------

procedure TUWTimeCodeHelper.EncodeTime(const Hour, Min, Secs, MSecs: Word);
begin
  Self := (Hour * (MinsPerHour * SecsPerMin   * MSecsPerSec)) +
          (Min  * SecsPerMin   * MSecsPerSec) +
          (Secs * MSecsPerSec) +
           MSecs;
end;

//------------------------------------------------------------------------------

procedure TUWTimeCodeHelper.DecodeTime(out Hour, Min, Secs, MSecs: Word);
var
  h, m, x: Integer;
begin
  h     := Self - (Hour*3600000);
  m     := Min * 60000;
  x     := h - m;

  Hour  := Trunc(Self / 3600000);
  Min   := Trunc(h / 60000);
  Secs  := Trunc(x / 1000);
  MSecs := Trunc(x - (Secs*1000));
end;

// -----------------------------------------------------------------------------

function TUWTimeCodeHelper.ToFrames(const FPS: Single): Cardinal;
begin
  Result := Round((Self / 1000) * FPS);
end;

//------------------------------------------------------------------------------

end.
