{*
 *  URUWorks Graphics
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

unit UWSystem.Graphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, LCLIntf;

procedure FillGradient(const ACanvas: TCanvas; const ARect: TRect; const FromColor, ToColor: Integer; ASteps: Byte = 128);

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

procedure FillGradient(const ACanvas: TCanvas; const ARect: TRect; const FromColor, ToColor: Integer; ASteps: Byte = 128);
var
  Deltas : array[0..2] of Real; // R,G,B
  i, Len : Integer;
  j      : Real;
  r      : TRect;
begin
  Len := (ARect.Right-ARect.Left);
  if ASteps > Len then ASteps := Len;

  Deltas[0] := (GetRValue(ToColor) - GetRValue(FromColor)) / ASteps;
  Deltas[1] := (GetGValue(ToColor) - GetGValue(FromColor)) / ASteps;
  Deltas[2] := (GetBValue(ToColor) - GetBValue(FromColor)) / ASteps;

  j := Len / ASteps;

  for i := 0 to ASteps - 1 do
  begin
    r := Rect(Round(i * j), ARect.Top, Round((i + 1) * j), ARect.Bottom);

    ACanvas.Brush.Color := RGB(
        Round(GetRValue(FromColor) + i * Deltas[0]),
        Round(GetGValue(FromColor) + i * Deltas[1]),
        Round(GetBValue(FromColor) + i * Deltas[2])
      );

    ACanvas.FillRect(r);
  end;
end;

// -----------------------------------------------------------------------------

end.

