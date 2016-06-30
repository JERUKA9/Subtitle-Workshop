{*
 *  URUWorks Subtitle Workshop
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

unit UFindAndReplace;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, UWControls;

type

  { TfrmFindAndReplace }

  TfrmFindAndReplace = class(TForm)
    Bevel1: TBevel;
    btnFindNext: TButton;
    btnGo: TButton;
    btnMoreLess: TButton;
    btnReplace: TButton;
    btnReplaceAll: TButton;
    btnFind: TButton;
    btnClose: TButton;
    cboFind: TComboBox;
    cboReplace: TComboBox;
    chkPreserveCaseOnReplace: TCheckBox;
    chkCaseSensitive: TCheckBox;
    chkUseRE: TCheckBox;
    lblFindTextIn: TLabel;
    lblTextToFind: TLabel;
    lblReplaceBy: TLabel;
    lblLineNumber: TLabel;
    pagFindAndReplace: TPageControl;
    rdoAllTheSubtitles: TRadioButton;
    rdoFromTheSelectedSubtitle: TRadioButton;
    tabFind: TTabSheet;
    tabReplace: TTabSheet;
    tabGoTo: TTabSheet;
    numGoTo: TUWNumberBox;
    lyoOptions: TUWLayout;
    procedure btnCloseClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnMoreLessClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pagFindAndReplaceChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmFindAndReplace: TfrmFindAndReplace;
  strLess,
  strMore: String;

// -----------------------------------------------------------------------------

implementation

uses UTypes, UCommon, UMain, jsonConf;

{$R *.lfm}

const
  _more = 361;
  _less = 208;

// -----------------------------------------------------------------------------

{ TfrmFindAndReplace }

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormCreate(Sender: TObject);
begin
  Height := _Less;

  cboFind.Text := Options.TextToFind;

  numGoTo.Min   := 1;
  numGoTo.Max   := Subtitles.Count;
  numGoTo.Value := VSTFocusedNode+1;

  if frmMain.VST.SelectedCount = 1 then
    rdoFromTheSelectedSubtitle.Checked := True
  else
    rdoAllTheSubtitles.Checked := True;

  ReadLangForForm(LanguageFileName, Self);

  with TJSONConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    with Options do
    begin
      OpenKey('FindAndReplace', True);
      chkCaseSensitive.Checked         := GetValue('chkCaseSensitive', False);
      chkPreserveCaseOnReplace.Checked := GetValue('chkPreserveCaseOnReplace', False);
      chkUseRE.Checked                 := GetValue('chkUseRE', False);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  with TJSONConfig.Create(NIL) do
  try
    FileName := SettingsFileName;
    with Options do
    begin
      OpenKey('FindAndReplace', True);
      SetValue('chkCaseSensitive', chkCaseSensitive.Checked);
      SetValue('chkPreserveCaseOnReplace', chkPreserveCaseOnReplace.Checked);
      SetValue('chkUseRE', chkUseRE.Checked);
      CloseKey;
    end;
    Flush;
  finally
    Free;
  end;

  CloseAction := caFree;
  frmFindAndReplace := NIL;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.pagFindAndReplaceChange(Sender: TObject);

  procedure SetCtrlsToTab(const Parent: TWinControl; const OnlyCancel: Boolean = False);
  begin
    btnClose.Parent := Parent;
    if not OnlyCancel then
    begin
      btnMoreLess.Parent   := Parent;
      lyoOptions.Parent    := Parent;
      lblTextToFind.Parent := Parent;
      cboFind.Parent       := Parent;
      if pagFindAndReplace.TabIndex  <> 2 then
        btnFindNext.Parent := Parent;

      if Self.Visible then
        case pagFindAndReplace.TabIndex of
          0, 1: cboFind.SetFocus;
          2   : numGoTo.SetFocus;
      end;
    end;
  end;

begin
  case pagFindAndReplace.TabIndex of
    0: SetCtrlsToTab(tabFind);
    1: SetCtrlsToTab(tabReplace);
    2: SetCtrlsToTab(tabGoTo, True);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnMoreLessClick(Sender: TObject);
begin
  with btnMoreLess do
    if Tag = 0 then
    begin
      Tag  := 1;
      Text := strLess;
      Self.Height := _more;
    end
    else
    begin
      Tag  := 0;
      Text := strMore;
      Self.Height := _less;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnFindClick(Sender: TObject);
begin
  VSTFind(cboFind.Text, chkCaseSensitive.Checked, not rdoFromTheSelectedSubtitle.Checked,
    False, '', False, False, chkUseRE.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnFindNextClick(Sender: TObject);
begin
  frmMain.actFindNext.Execute;
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnReplaceClick(Sender: TObject);
begin
  VSTFind(cboFind.Text, chkCaseSensitive.Checked, not rdoFromTheSelectedSubtitle.Checked,
    True, cboReplace.Text, False, chkPreserveCaseOnReplace.Checked, chkUseRE.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnReplaceAllClick(Sender: TObject);
begin
  VSTFind(cboFind.Text, chkCaseSensitive.Checked, not rdoFromTheSelectedSubtitle.Checked,
    True, cboReplace.Text, True, chkPreserveCaseOnReplace.Checked, chkUseRE.Checked);
end;

// -----------------------------------------------------------------------------

procedure TfrmFindAndReplace.btnGoClick(Sender: TObject);
begin
  VSTSelectNode(numGoTo.Value-1, True);
end;

// -----------------------------------------------------------------------------

end.

