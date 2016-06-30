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

unit UWSubtitleAPI.Formats.AdvancedSubtitles;

// -----------------------------------------------------------------------------

interface

uses
  SysUtils, StrUtils, UWSubtitleAPI, UWSystem.TimeUtils, UWSystem.StrUtils,
  UWSubtitleAPI.Formats, Classes, laz2_XMLRead, laz2_DOM, laz2_XMLWrite;

type

  { TUWAdvancedSubtitles }

  TUWAdvancedSubtitles = class(TUWSubtitleCustomFormat)
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

uses UWSubtitleAPI.ExtraInfo, UWSubtitleAPI.Tags, UWSystem.XmlUtils, LCLIntf;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.Name: String;
begin
  Result := IndexToName(Integer(Format));
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.Format: TUWSubtitleFormats;
begin
  Result := TUWSubtitleFormats.sfAdvancedSubtitles;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.Extension: String;
begin
  Result := '*.xas';
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.IsTimeBased: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.HasStyleSupport: Boolean;
begin
  Result := True; // Has tags
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.IsMine(const SubtitleFile: TUWStringList; const Row: Integer): Boolean;
begin
  if Contains('dvddisc/ADV_OBJ/', SubtitleFile[Row]) then
    Result := True
  else
    Result := False;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.LoadSubtitle(const SubtitleFile: TUWStringList; const FPS: Single; var Subtitles: TUWSubtitles): Boolean;
var
  XmlDoc      : TXMLDocument;
  Node        : TDOMNode;
  InitialTime : Integer;
  FinalTime   : Integer;
  Text        : String;
  Style       : String;
begin
  Result := False;
  XmlDoc := NIL;
  ReadXMLFile(XmlDoc, SubtitleFile.FileName);
  if Assigned(XmlDoc) then
    try
      // Styles
      Node := XMLFindNodeByName(XmlDoc, 'style');
      if Assigned(Node) then
      begin
        Style := Node.Attributes.GetNamedItem('id').NodeValue;
        // Subtitles
        Node := XMLFindNodeByName(XmlDoc, 'p');
        if Assigned(Node) then
          repeat
            if Style = Node.Attributes.GetNamedItem('style').NodeValue then
            begin
              InitialTime := StringToTime(Node.Attributes.GetNamedItem('begin').NodeValue);
              FinalTime   := StringToTime(Node.Attributes.GetNamedItem('end').NodeValue);
              Text        := ReplaceEnters(Node.TextContent, '<br/>', sLineBreak);
              Subtitles.Add(InitialTime, FinalTime, HTMLTagsToSW(Text), '', NIL, False);
            end;
            Node := Node.NextSibling;
          until (Node = NIL);
      end;
    finally
      XmlDoc.Free;
      Result := Subtitles.Count > 0;
    end;
end;

// -----------------------------------------------------------------------------

function TUWAdvancedSubtitles.SaveSubtitle(const FileName: String; const FPS: Single; const Encoding: TEncoding; const Subtitles: TUWSubtitles; const FromItem: Integer = -1; const ToItem: Integer = -1): Boolean;

  function AlphaColorToRGBAString(const Color: Cardinal): String;
  begin
    Result := SysUtils.Format('rgba(%d, %d, %d, %d)', [GetRValue(Color), GetGValue(Color), GetBValue(Color), $FF]);
  end;

  function ByteToTextAlignString(const Alignment: Byte): String;
  begin
    case Alignment of
      1: Result := 'Left';
      2: Result := 'Right';
    else
      Result := 'Center'
    end;
  end;

var
  XmlDoc : TXMLDocument;
  Root, Element, Node, SubNode : TDOMNode;
  i : Integer;
begin
  Result := False;
  XmlDoc := TXMLDocument.Create;
  try
    if Subtitles.Header <> NIL then
    begin
      with PXAS_Info(Subtitles.Header)^ do
      begin
        Root := XmlDoc.CreateElement('root');
          TDOMElement(Root).SetAttribute('xml:lang', Language);
          TDOMElement(Root).SetAttribute('xmlns', 'http://www.dvdforum.org/2005/ihd');
          TDOMElement(Root).SetAttribute('xmlns:style', 'http://www.dvdforum.org/2005/ihd#style');
          TDOMElement(Root).SetAttribute('xmlns:state', 'http://www.dvdforum.org/2005/ihd#state');
          XmlDoc.Appendchild(Root);
        Root := XmlDoc.DocumentElement;

        Element := XmlDoc.CreateElement('head');
        Root.AppendChild(Element);

        Element := XmlDoc.CreateElement('styling');
        Root.AppendChild(Element);

        Element := XmlDoc.CreateElement('style');
          TDOMElement(Root).SetAttribute('id', 'swText');
          TDOMElement(Root).SetAttribute('style:font', 'file:///dvddisc/ADV_OBJ/' + FontName);
          TDOMElement(Root).SetAttribute('style:fontSize', IntToStr(FontSize) + 'px');
          TDOMElement(Root).SetAttribute('style:color', AlphaColorToRGBAString(FontColor));
          TDOMElement(Root).SetAttribute('style:textAlign', ByteToTextAlignString(Alignment));
        Root.AppendChild(Element);

        Element := XmlDoc.CreateElement('body');
          TDOMElement(Root).SetAttribute('xml:base', 'file:///dvddisc/ADV_OBJ/');
        Root.AppendChild(Element);

        Element := XmlDoc.CreateElement('div');
          TDOMElement(Root).SetAttribute('style:position', 'absolute');
          TDOMElement(Root).SetAttribute('style:x', IntToStr(X) + '%');
          TDOMElement(Root).SetAttribute('style:y', IntToStr(Y) + '%');
          TDOMElement(Root).SetAttribute('style:width', IntToStr(W) + '%');
          TDOMElement(Root).SetAttribute('style:height', IntToStr(H) + '%');
        Root.AppendChild(Element);

        Node := Element;
      end;
    end
    else
    begin
      Root := XmlDoc.CreateElement('root');
      XmlDoc.Appendchild(Root);
      Root := XmlDoc.DocumentElement;
      Node := Root;
    end;

    for i := FromItem to ToItem do
    begin
      Element := XmlDoc.CreateElement('p');
      TDOMElement(Element).SetAttribute('style', 'swText');
      TDOMElement(Element).SetAttribute('begin', TimeToString(Subtitles.InitialTime[i], 'hh:mm:ss:zz'));
      TDOMElement(Element).SetAttribute('end', TimeToString(Subtitles.FinalTime[i], 'hh:mm:ss:zz'));
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

function TUWAdvancedSubtitles.ToText(const Subtitles: TUWSubtitles): String;
begin
  Result := '';
end;

// -----------------------------------------------------------------------------

end.
