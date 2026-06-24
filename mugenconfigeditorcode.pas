unit mugenconfigeditorcode;

{
 This software was made by Popov Evgeniy Alekseyevich.
 It is distributed under the GNU GENERAL PUBLIC LICENSE (Version 2 or higher).
}

{$mode objfpc}
{$H+}

interface

uses Classes, LCLProc, LCLType, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, LazFileUtils, SynEdit, SynHighlighterIni;

type

  { TMainWindow }

    TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    EditMenu: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ConfigurationHighLighter: TSynIniSyn;
    Editor: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure SelectAllMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure CutMenuItemClick(Sender: TObject);
    procedure SaveDialogCanClose(Sender: TObject; var CanClose: boolean);
  private
    document:string;
    procedure create_new_file();
    procedure load_file(const target:string);
    procedure save_file(const target:string);
    procedure window_setup();
    procedure window_resize();
    procedure editor_setup();
    procedure dialog_setup();
    procedure set_shortcut();
    procedure setup();
  public
    { public declarations }
  end;

var MainWindow: TMainWindow;

implementation

function get_file_extension(const filter:Byte):string;
var extension:array[1..5] of string=('.def','.cfg','.cns','.air','.cmd');
begin
 get_file_extension:=extension[filter];
end;

procedure TMainWindow.create_new_file();
begin
 Self.Editor.Lines.Clear();
 Self.document:='';
end;

procedure TMainWindow.load_file(const target:string);
begin
 try
  Self.Editor.Lines.LoadFromFile(target);
  Self.Editor.Modified:=False;
  Self.document:=target;
 except
  On E:Exception do ShowMessage(E.Message);
 end;

end;

procedure TMainWindow.save_file(const target:string);
begin
 try
  Self.Editor.Lines.SaveToFile(target);
  Self.Editor.Modified:=False;
  Self.document:=target;
 except
  On E:Exception do ShowMessage(E.Message);
 end;

end;

procedure TMainWindow.window_setup();
begin
 Application.Title:='Mugen config editor';
 Self.Caption:=Application.Title;
 Self.Font.Name:=Screen.MenuFont.Name;
 Self.Font.Size:=14;
end;

procedure TMainWindow.window_resize();
begin
 Self.Editor.Width:=Self.ClientWidth-10;
 Self.Editor.Height:=Self.ClientHeight-10;
end;

procedure TMainWindow.editor_setup();
begin
 Self.ConfigurationHighLighter.DefaultFilter:=Self.OpenDialog.Filter;
 Self.ConfigurationHighLighter.Enabled:=True;
 Self.Editor.Highlighter:=Self.ConfigurationHighLighter;
 Self.Editor.ScrollBars:=ssBoth;
end;

procedure TMainWindow.dialog_setup();
begin
 Self.OpenDialog.Title:='Open a mugen config file';
 Self.SaveDialog.Title:='Save a mugen config file';
 Self.OpenDialog.Filter:='Mugen config files|*.def;*.cns;*.air;*.cmd;*.cfg';
 Self.SaveDialog.Filter:='Game settings|*.def|Configuration file|*.cfg|Character definitive|*.cns|Animation settings|*.air|AI settings|*.cmd';
end;

procedure TMainWindow.set_shortcut();
begin
 Self.NewMenuItem.ShortCut:=TextToShortCut('Ctrl+N');
 Self.OpenMenuItem.ShortCut:=TextToShortCut('Ctrl+O');
 Self.SaveMenuItem.ShortCut:=TextToShortCut('Ctrl+S');
 Self.SaveAsMenuItem.ShortCut:=TextToShortCut('Ctrl+Alt+S');
 Self.SelectAllMenuItem.ShortCut:=TextToShortCut('Ctrl+A');
 Self.CopyMenuItem.ShortCut:=TextToShortCut('Ctrl+C');
 Self.PasteMenuItem.ShortCut:=TextToShortCut('Ctrl+V');
 Self.CutMenuItem.ShortCut:=TextToShortCut('Ctrl+X');
end;

procedure TMainWindow.setup();
begin
 Self.window_setup();
 Self.dialog_setup();
 Self.editor_setup();
 Self.set_shortcut();
 Self.window_resize();
 Self.create_new_file();
end;

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
 Self.setup();
end;

procedure TMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if Self.Editor.Modified=True then
 begin
  if Application.MessageBox('Do you want to save changes?','Save a file',MB_ICONQUESTION+MB_YESNO)=ID_YES then Self.SaveMenuItem.Click();
 end;

end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
 Self.window_resize();
end;

procedure TMainWindow.AboutMenuItemClick(Sender: TObject);
begin
 ShowMessage('Mugen config editor. Version 2.0. 2007-2026 years. This software was made by Popov Evgeniy Alekseyevich');
end;

procedure TMainWindow.NewMenuItemClick(Sender: TObject);
begin
 Self.create_new_file();
end;

procedure TMainWindow.OpenMenuItemClick(Sender: TObject);
begin
 if Self.OpenDialog.Execute()=True then Self.load_file(Self.OpenDialog.FileName);
end;

procedure TMainWindow.SaveMenuItemClick(Sender: TObject);
begin
 if Self.document<>'' then
 begin
  Self.save_file(Self.document);
 end
 else
 begin
  Self.SaveDialog.Execute();
 end;

end;

procedure TMainWindow.SaveAsMenuItemClick(Sender: TObject);
begin
 Self.SaveDialog.Execute();
end;

procedure TMainWindow.SelectAllMenuItemClick(Sender: TObject);
begin
 Self.Editor.SelectAll();
end;

procedure TMainWindow.CopyMenuItemClick(Sender: TObject);
begin
 try
  Self.Editor.CopyToClipboard();
 except
  On E:Exception do ShowMessage(E.Message);
 end;

end;

procedure TMainWindow.PasteMenuItemClick(Sender: TObject);
begin
 try
  Self.Editor.PasteFromClipboard();
 except
  On E:Exception do ShowMessage(E.Message);
 end;

end;

procedure TMainWindow.CutMenuItemClick(Sender: TObject);
begin
 try
  Self.Editor.CutToClipboard();
 except
  On E:Exception do ShowMessage(E.Message);
 end;

end;

procedure TMainWindow.SaveDialogCanClose(Sender: TObject; var CanClose: boolean);
begin
 Self.save_file(ExtractFileNameWithoutExt(Self.SaveDialog.FileName)+get_file_extension(Self.SaveDialog.FilterIndex));
end;

end.
