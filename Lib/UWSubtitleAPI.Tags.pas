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

unit UWSubtitleAPI.Tags;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils;

{ Tags Functions }

function RemoveSWTags(const Text: String): String;
function SWTagsToHTML(const Text: String): String;
function HTMLTagsToSW(const Text: String): String;
function SWTagsToMicroDVD(const Text: String): String;
function MicroDVDTagsToSW(const Text: String): String;

// -----------------------------------------------------------------------------

implementation

uses UWSystem.StrUtils, RegExpr;

// -----------------------------------------------------------------------------

{ Tags functions }

//------------------------------------------------------------------------------

function RemoveSWTags(const Text: String): String;
begin
  //Result := Text;
  Result := ReplaceRegExpr('\{.*?\}', Text, '', True);
end;

//------------------------------------------------------------------------------

function SWTagsToHTML(const Text: String): String;
begin
  Result := Text;
  Result := ReplaceRegExpr('\{C:(.*?|var)\}', Result, '<font color=$1>', True);
  Result := ReplaceRegExpr('\{/C\}', Result, '</font>', True);
//  Result := ReplaceRegExpr('\{(.*?|var)\}', Result, '<$1>', True);
  Result := ReplaceString(Result, '{B}', '<b>');
  Result := ReplaceString(Result, '{I}', '<i>');
  Result := ReplaceString(Result, '{U}', '<u>');
  Result := ReplaceString(Result, '{S}', '<s>');
  Result := ReplaceString(Result, '{/B}', '</b>');
  Result := ReplaceString(Result, '{/I}', '</i>');
  Result := ReplaceString(Result, '{/U}', '</u>');
  Result := ReplaceString(Result, '{/S}', '</s>');
end;

// -----------------------------------------------------------------------------

function HTMLTagsToSW(const Text: String): String;
begin
  Result := Text;
  Result := ReplaceRegExpr('<font color=(.*?|var)>', Result, '{C:$1}', True);
  Result := ReplaceRegExpr('</font>', Result, '{/C}', True);
//  Result := ReplaceRegExpr('<(.*?|var)>', Result, '{$1}', True);
  Result := ReplaceString(Result, '<br>', sLineBreak);
  Result := ReplaceString(Result, '</br>', sLineBreak);
  Result := ReplaceString(Result, '<b>', '{B}');
  Result := ReplaceString(Result, '<i>', '{I}');
  Result := ReplaceString(Result, '<u>', '{U}');
  Result := ReplaceString(Result, '<s>', '{S}');
  Result := ReplaceString(Result, '</b>', '{/B}');
  Result := ReplaceString(Result, '</i>', '{/I}');
  Result := ReplaceString(Result, '</u>', '{/U}');
  Result := ReplaceString(Result, '</s>', '{/S}');
end;

// -----------------------------------------------------------------------------

function SWTagsToMicroDVD(const Text: String): String;
begin
  Result := Text;
  Result := ReplaceString(Result, '{I}', '{y:i}');
  Result := ReplaceString(Result, '{/I}', '');
  Result := ReplaceString(Result, '{B}', '{y:b}');
  Result := ReplaceString(Result, '{/B}', '');
  Result := ReplaceString(Result, '{U}', '{y:u}');
  Result := ReplaceString(Result, '{/U}', '');
  Result := ReplaceString(Result, '{S}', '{y:s}');
  Result := ReplaceString(Result, '{/S}', '');
end;

// -----------------------------------------------------------------------------

function MicroDVDTagsToSW(const Text: String): String;
begin
  Result := Text;
  Result := ReplaceString(Result, '{y:i}', '{I}');
  Result := ReplaceString(Result, '{y:b}', '{B}');
  Result := ReplaceString(Result, '{y:u}', '{U}');
  Result := ReplaceString(Result, '{y:s}', '{S}');
  Result := ReplaceRegExpr('{c:$(.*?|var)}', Result, '{C:$1}', True);
  Result := ReplaceRegExpr('{(.*?)}', Result, '', True);
end;

// -----------------------------------------------------------------------------

end.
