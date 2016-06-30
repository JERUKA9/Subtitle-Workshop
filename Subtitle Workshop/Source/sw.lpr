program sw;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, virtualtreeview_package, PasLibVlcPlayer, UMain, utypes, ucommon,
  UUndo, UErrors, UWSystem.Graphics, pascalscript, ufindandreplace, uspellcheck,
  uabout, uwelcome, utimings, utexts
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Subtitle Workshop';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

