{*
 *  URUWorks StrUtils
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

unit UWSystem.StrUtils;

// -----------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, StrUtils, RegExpr;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: String): String; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;

function Contains(const AFind, ASource: String): Boolean;

function ReplaceString(const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
function ReplaceEnters(const S: String; const OldPattern: String = sLineBreak; const NewPattern: String = '|'): String;
function StringCount(const AFindString, ASourceString: String): Integer;
function WordCount(const Text: String): Integer;
function LineCount(const Text: String; const Pattern: String = sLineBreak): Cardinal;
function IsUpperCase(const Text: String): Boolean;
function PreserveCase(const Text, NewText: String): String;
function SentenceCase(const Text: String): String;
function InvertCase(const Text: String): String;
function TitleCase(const Text: String): String;
function PosRE(const ARExpr, ASource: String): Integer;
function PosCS(const SubStr, Str: String): Integer;
function LastPos(const SubStr, S: String): Integer;
function RemoveTagsFromText(const Text: String): String;
function ReplaceWordString(const Text, Word, NewWord: String; const ReplaceAll: Boolean = False): String;
procedure SplitStr(const Delimiter, Text: String; var Pieces: TStringList);
procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);

// -----------------------------------------------------------------------------

implementation

//------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: String): String;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

// -----------------------------------------------------------------------------

function Contains(const AFind, ASource: String): Boolean;
begin
  Result := Pos(AFind, ASource) > 0;
end;

// -----------------------------------------------------------------------------

function ReplaceString(const S, OldPattern, NewPattern: String; ReplaceAll: Boolean = True; IgnoreCase: Boolean = True): String;
var
  Flags : TReplaceFlags;
begin
  Flags := [];
  if ReplaceAll then Flags := Flags + [rfReplaceAll];
  if IgnoreCase then Flags := Flags + [rfIgnoreCase];
  Result := StringReplace(S, OldPattern, NewPattern, Flags);
end;

// -----------------------------------------------------------------------------

function ReplaceEnters(const S: String; const OldPattern: String = sLineBreak; const NewPattern: String = '|'): String;
begin
  Result := ReplaceString(S, OldPattern, NewPattern);
end;

// -----------------------------------------------------------------------------

function StringCount(const AFindString, ASourceString: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  i      := 1;
  repeat
    i := PosEx(AFindString, ASourceString, i);
    if i > 0 then
    begin
      Inc(i);
      Inc(Result);
    end;
  until i = 0;
end;

// -----------------------------------------------------------------------------

function WordCount(const Text: String): Integer;
begin
  StrUtils.WordCount(Text, StrUtils.WordDelimiters);
end;

// -----------------------------------------------------------------------------

function LineCount(const Text: String; const Pattern: String = sLineBreak): Cardinal;
begin
  Result := StringCount(Pattern, Text) + 1;
end;

// -----------------------------------------------------------------------------

function IsUpperCase(const Text: String): Boolean;
begin
  Result := Text = UpperCase(Text);
end;

// -----------------------------------------------------------------------------

function PreserveCase(const Text, NewText: String): String;
var
  i,
  l1,
  l2: Integer;
begin
  Result := NewText;
  l1     := Length(Text);
  l2     := Length(NewText);
  if l1 = l2 then
  begin
    for i := 1 to l1 do
      if IsUpperCase(Text[i]) then
        Result[i] := UpperCase(NewText)[i]
      else
        Result[i] := LowerCase(NewText[i]);
  end
  else if IsUpperCase(Text) then
    Result := UpperCase(NewText);
end;

// -----------------------------------------------------------------------------

function SentenceCase(const Text: String): String;
var
  s: String;
begin
  Result := LowerCase(Text);
  if Result = '' then Exit;
  s := UpperCase(Text);
  Result[1] := s[1];
end;

// -----------------------------------------------------------------------------

function InvertCase(const Text: String): String;
var
  sl,
  su : String;
  i  : Integer;
begin
  Result := Text;
  if Result = '' then Exit;
  sl := LowerCase(Text);
  su := UpperCase(Text);
  for i := 0 to Length(Text) - 1 do
    if Result[i] = sl[i] then
      Result[i] := su[i]
    else
      Result[i] := sl[i];
end;

// -----------------------------------------------------------------------------

function TitleCase(const Text: String): String;
var
  su : String;
  i  : Integer;
  up : Boolean;
begin
  Result := LowerCase(Text);
  if Result = '' then Exit;
  su := UpperCase(Text);
  up := True;
  i  := 1;
  while i <= Length(Text) do
  begin
    while (i <= Length(Text)) and (Text[i] in StrUtils.WordDelimiters) do
    begin
      Inc(i);
      up := True;
    end;
    if up and not (Text[i] in StrUtils.WordDelimiters) then
    begin
      Result[i] := su[i];
      up := False;
    end;
    Inc(i);
  end;
end;

// -----------------------------------------------------------------------------

function PosRE(const ARExpr, ASource: String): Integer;
begin
  with TRegExpr.Create do
  try
    Expression := ARExpr;
    Exec(ASource);
    Result := MatchPos[0]; // -1 = not found
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

function PosCS(const SubStr, Str: String): Integer;
begin
  Result := Pos(AnsiLowerCase(SubStr), AnsiLowerCase(Str));
end;

// -----------------------------------------------------------------------------

function LastPos(const SubStr, S: String): Integer;
var
  Found, Len, Pos: Integer;
begin
  Pos   := Length(S);
  Len   := Length(SubStr);
  Found := 0;
  while (Pos > 0) and (Found = 0) do
  begin
    if Copy(S, Pos, Len) = SubStr then Found := Pos;
    Dec(Pos);
  end;
  LastPos := Found;
end;

// -----------------------------------------------------------------------------

function RemoveTagsFromText(const Text: String): String;
var
  a, b: Integer;
begin
  Result := Trim(Text);
  repeat
    a := Pos('<', Result);
    b := Pos('>', Result);
    if (a > 0) and (b > 0) then
    begin
      Delete(Result, a, b);
    end;
  until (a <= 0) or (b <= 0);
end;

// -----------------------------------------------------------------------------

function ReplaceWordString(const Text, Word, NewWord: String; const ReplaceAll: Boolean = False): String;

  function IsValidToCut(const C: Char): Boolean;
  begin
    Result := not CharInSet(C, ['0'..'9', 'a'..'z', 'A'..'Z']);
  end;

var
  x: Integer;
begin
  Result := Text;

  x := Pos(Word, Result);
  if x > 0 then
  begin
    if Result = Word then
      Result := NewWord
    else if (x = 1) and ((x+Length(Word)) <= Length(Result)) then
    begin
      if IsValidToCut( Result[x+Length(Word)] ) then
      begin
        Insert(NewWord, Result, x);
        Delete(Result, x+Length(NewWord), Length(Word));

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end
    else if (x > 1) and ((x+Length(Word))-1 = Length(Result)) then
    begin
      if IsValidToCut( Result[x-1] ) then
      begin
        Delete(Result, x, Length(Word));
        Result := Result + NewWord;

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end
    else if (x > 1) and ((x+Length(Word))-1 < Length(Result)) then
    begin
      if IsValidToCut( Result[x-1] ) and IsValidToCut( Result[x+Length(Word)] ) then
      begin
        Insert(NewWord, Result, x);
        Delete(Result, x+Length(NewWord), Length(Word));

        if ReplaceAll then Result := ReplaceWordString(Result, Word, NewWord);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SplitStr(const Delimiter, Text: String; var Pieces: TStringList);
begin
  if Text <> '' then
  begin
    if (Pieces = NIL) then Pieces := TStringList.Create;
    Pieces.Text := StringReplace(Text, Delimiter, sLineBreak, [rfReplaceAll]);
  end;
end;

// -----------------------------------------------------------------------------

procedure AnsiStringToAnsiChar(var Dest: array of AnsiChar; const Source: AnsiString);
var
  i, MaxLen: Integer;
begin
  MaxLen := High(Dest);

  for i := 0 to MaxLen do
    Dest[i] := #0;

  if MaxLen > Length(Source) then MaxLen := Length(Source)-1;

  for i := 0 to MaxLen do
    Dest[i] := Source[i+1];
end;

// -----------------------------------------------------------------------------

end.
