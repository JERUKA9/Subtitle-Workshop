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

unit UWSubtitleAPI.Formats.ABCiView;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, StrUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSubtitleAPI.Formats, Classes, laz2_XMLRead, laz2_DOM, laz2_XMLWrite;

type

  { TUWABCiView }

  TUWABCiView = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags, UWSystem.XmlUtils;

// -----------------------------------------------------------------------------

function TUWABCiView.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWABCiView.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfABCiView;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.Extension: String;
begin
  Result := '*.xml';
end;

// -----------------------------------------------------------------------------

function TUWABCiView.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWABCiView.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if Contains('<reel ', SubtitleFile[Row]) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  XmlDoc      : TXMLDocument;
  Node        : TDOMNode;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
begin
  Result := False;
  XmlDoc := NIL;
  ReadXMLFile(XmlDoc, SubtitleFile.FileName);
  if Assigned(XmlDoc) then
    try
      Node := XMLFindNodeByName(XmlDoc, 'title');
      if Assigned(Node) then
        repeat
          InitialTime := StringToTime(Node.Attributes.GetNamedItem('start').NodeValue);
          FinalTime   := StringToTime(Node.Attributes.GetNamedItem('end').NodeValue);
          Text        := ReplaceEnters(Node.TextContent, '<br/>', sLineBreak);
          Subtitles.Add(InitialTime, FinalTime, HTMLTagsToSW(Text), '', NIL, False);

          Node := Node.NextSibling;
        until (Node = NIL);
    finally
       XmlDoc.Free;
       Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;
var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  try
    Root := XmlDoc.CreateElement('root');
      TDOMElement(Root).SetAttribute('fps', '25');
      TDOMElement(Root).SetAttribute('movie', 'program title');
      TDOMElement(Root).SetAttribute('language', 'GBR:English (UK)');
      TDOMElement(Root).SetAttribute('font', 'Arial');
      TDOMElement(Root).SetAttribute('style', 'normal');
      TDOMElement(Root).SetAttribute('size', '48');
      XmlDoc.Appendchild(Root);
    Root := XmlDoc.DocumentElement;

    Element := XmlDoc.CreateElement('reel');
      TDOMElement(Element).SetAttribute('start', '');
      TDOMElement(Element).SetAttribute('first', '');
      TDOMElement(Element).SetAttribute('last', '');
    Root.AppendChild(Element);
    Node := Element;

    for i := FromItem to ToItem do
    begin
      Element := XmlDoc.CreateElement('title');
      TDOMElement(Element).SetAttribute('start', TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zzz'));
      TDOMElement(Element).SetAttribute('end', TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zzz'));
      SubNode := XmlDoc.CreateTextNode(SWTagsToHTML(ReplaceEnters(Subtitles.Text[i], sLineBreak, '<br/>')));
      Element.AppendChild(SubNode);
      Node.AppendChild(Element);
    end;

    try
      WriteXMLFile(XmlDoc, FileName);
    except
    end;
  finally
    XmlDoc.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TUWABCiView.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
