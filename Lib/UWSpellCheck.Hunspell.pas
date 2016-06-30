{*
 *  URUWorks Hunspell
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

unit UWSpellCheck.Hunspell;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, dynlibs;

type

  { TUWHunspell }

  TUWHunspell = class
  private
    hunspell_initialize   : function(aff_file: PChar; dict_file: PChar): Pointer; cdecl;
    hunspell_uninitialize : procedure(spell: Pointer); cdecl;
    hunspell_spell        : function(spell: Pointer; word: PChar): Boolean; cdecl;
    hunspell_suggest      : function(spell: Pointer; word: PChar; var suggestions: PPChar): Integer; cdecl;
    hunspell_suggest_free : procedure(spell: Pointer; suggestions: PPChar; suggestLen: Integer); cdecl;
    hunspell_put_word     : function(spell: Pointer; word: PChar): Integer; cdecl;
    FHandle : TLibHandle;
    FSpell  : Pointer;
  public
    constructor Create(const LibraryName: String = {$IFDEF MSWINDOWS}{$IFDEF WIN32}'libhunspellx86.dll'{$ELSE}'libhunspellx64.dll'{$ENDIF}{$ELSE}'libhunspell.so'{$ENDIF});
    destructor Destroy; override;
    function LoadHunspell(const LibraryName: String): Boolean;
    function Ready: Boolean;
    function LoadDictionary(const aff, dict: String): Boolean;
    procedure UnloadDictionary;
    function Spell(const Text: String): Boolean;
    function Suggest(const Text: String; var Suggests: TStrings): Boolean;
    procedure Add(const Word: String);
  end;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

constructor TUWHunspell.Create(const LibraryName: String = {$IFDEF MSWINDOWS}{$IFDEF WIN32}'libhunspellx86.dll'{$ELSE}'libhunspellx64.dll'{$ENDIF}{$ELSE}'libhunspell.so'{$ENDIF});
begin
  inherited Create;

  FHandle := 0;
  FSpell  := NIL;
  LoadHunspell(LibraryName);
end;

// -----------------------------------------------------------------------------

destructor TUWHunspell.Destroy;
begin
  UnloadDictionary;

  if FSpell <> NIL then FSpell := NIL;
  if FHandle <> 0 then FreeLibrary(FHandle);

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.LoadHunspell(const LibraryName: String): Boolean;
begin
  Result := False;
  if (LibraryName = '') or (FHandle <> 0) or not FileExists(LibraryName) then Exit;

  FHandle := LoadLibrary(LibraryName);
  if FHandle <> 0 then
  begin
    Pointer(hunspell_initialize) := GetProcAddress(FHandle, 'hunspell_initialize');
    if not Assigned(hunspell_initialize) then Exit;
    Pointer(hunspell_uninitialize) := GetProcAddress(FHandle, 'hunspell_uninitialize');
    if not Assigned(hunspell_uninitialize) then Exit;
    Pointer(hunspell_spell) := GetProcAddress(FHandle, 'hunspell_spell');
    if not Assigned(hunspell_spell) then Exit;
    Pointer(hunspell_suggest) := GetProcAddress(FHandle, 'hunspell_suggest');
    if not Assigned(hunspell_suggest) then Exit;
    Pointer(hunspell_suggest_free) := GetProcAddress(FHandle, 'hunspell_suggest_free');
    if not Assigned(hunspell_suggest_free) then Exit;
    Pointer(hunspell_put_word) := GetProcAddress(FHandle, 'hunspell_put_word');
    if not Assigned(hunspell_put_word) then Exit;

    Result  := True;
  end;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.Ready: Boolean;
begin
  Result := FHandle <> 0;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.LoadDictionary(const aff, dict: String): Boolean;
begin
  Result := False;
  if not FileExists(aff) or not FileExists(dict) or not Assigned(hunspell_initialize) then Exit;

  FSpell := hunspell_initialize(PChar(String(aff)), PChar(String(dict)));
  Result := FSpell <> NIL;
end;

// -----------------------------------------------------------------------------

procedure TUWHunspell.UnloadDictionary;
begin
  if Assigned(hunspell_uninitialize) and (FSpell <> NIL) then
  begin
    hunspell_uninitialize(FSpell);
    FSpell := NIL;
  end;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.Spell(const Text: String): Boolean;
begin
  if Assigned(FSpell) and Assigned(hunspell_spell) then
    Result := hunspell_spell(FSpell, PChar(String(Text)))
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWHunspell.Suggest(const Text: String; var Suggests: TStrings): Boolean;
var
  l, i : Integer;
  s    : PPChar;
begin
  if (Suggests = NIL) then Suggests := TStringList.Create;
  Suggests.Clear;

  if Assigned(FSpell) and Assigned(hunspell_suggest) then
  begin
    l := hunspell_suggest(FSpell, PChar(String(Text)), s);
    for i := 0 to Pred(l) do
      Suggests.Add(String(s[i]));

    hunspell_suggest_free(FSpell, s, l);
  end;

  Result := Suggests.Count > 0;
end;

// -----------------------------------------------------------------------------

procedure TUWHunspell.Add(const Word: String);
begin
  if Assigned(FSpell) and Assigned(hunspell_put_word) then
    if Word <> '' then hunspell_put_word(FSpell, PChar(String(Word)));
end;

// -----------------------------------------------------------------------------

end.
