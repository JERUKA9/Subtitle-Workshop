{*
 *  URUWorks XML Utils
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

unit UWSystem.XmlUtils;

// -----------------------------------------------------------------------------

interface

uses
  laz2_XMLRead, laz2_DOM;

function XMLFindNodeByName(const XmlDoc: TXMLDocument; const NodeName: String): TDOMNode;

// -----------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------

function XMLFindNodeByName(const XmlDoc: TXMLDocument; const NodeName: String): TDOMNode;

  function FindNode(ANode: TDOMNode): TDOMNode;
  var
    i: Integer;
  begin
    Result := NIL;
    if Assigned(ANode) then
    begin
      if ANode.NodeName = NodeName then
        Result := ANode
      else
        for i := 0 to ANode.ChildNodes.Count-1 do
        begin
          Result := FindNode(ANode.ChildNodes[i]);
          if Assigned(Result) then Break;
        end;
    end;
  end;

var
  i: Integer;
begin
  Result := NIL;
  if Assigned(XmlDoc) and (XmlDoc.ChildNodes.Count > 0) then
  begin
    for i := 0 to XmlDoc.ChildNodes.Count-1 do
    begin
      Result := FindNode(XmlDoc.ChildNodes[i]);
      if Assigned(Result) then Break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

end.
