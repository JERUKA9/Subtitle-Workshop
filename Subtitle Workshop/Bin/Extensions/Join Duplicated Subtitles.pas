// -------------------------------------------------------------------------- //
//          Subtitle Workshop - join duplicated subtitles extension           //
//                          Copyright © 2016 URUWorks                         //
//                             http://uruworks.net                            //
//                                                                            //
//  The idea is to keep the initial time and the text of the first subtitle   //
//  with the final time of the second subtitle, joining both subtitles into   //
//  a single one.                                                             //
//                                                                            //
// -------------------------------------------------------------------------- //

program JoinDuplicatedSubtitles;

// -----------------------------------------------------------------------------

var
  i, p, c : Integer;
begin
  c := GetSubtitleCount;
  if not c > 1 then Exit;

  i := c-1;
  while i > 0 do
  begin
    p := i-1;
    if GetSubtitleText(i) = GetSubtitleText(p) then
    begin    
      SetSubtitleFinalTime(p, GetSubtitleFinalTime(i));
      DeleteSubtitle(i);
    end;
    i := p;
  end;
end.

// -----------------------------------------------------------------------------

end.
