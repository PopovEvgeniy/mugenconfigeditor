unit mugenconfigeditorcode;

{
 This software was made by Popov Evgeniy Alekseyevich.
 It is distributed under the GNU GENERAL PUBLIC LICENSE (Version 2 or higher).
}

{$mode objfpc}
{$H+}

interface

uses Classes,LCLProc,LCLType,SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, LazFileUtils;

type

  { TMainWindow }

    TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    Editor: TMemo;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    ClipboardMenu: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure CutMenuItemClick(Sender: TObject);
    procedure OpenDialogCanClose(Sender: TObject; var CanClose: boolean);
    procedure SaveDialogCanClose(Sender: TObject; var CanClose: boolean);
  private
    procedure window_setup();
    procedure window_resize();
    procedure interface_setup();
    procedure dialog_setup();
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

procedure TMainWindow.interface_setup();
begin
 Self.OpenDialog.FileName:='';
 Self.Editor.ScrollBars:=ssBoth;
 Self.NewMenuItem.ShortCut:=TextToShortCut('Ctrl+N');
 Self.OpenMenuItem.ShortCut:=TextToShortCut('Ctrl+O');
 Self.SaveMenuItem.ShortCut:=TextToShortCut('Ctrl+S');
 Self.SaveAsMenuItem.ShortCut:=TextToShortCut('Ctrl+Alt+S');
 Self.CopyMenuItem.ShortCut:=TextToShortCut('Ctrl+C');
 Self.PasteMenuItem.ShortCut:=TextToShortCut('Ctrl+V');
 Self.CutMenuItem.ShortCut:=TextToShortCut('Ctrl+X');
 Self.Editor.Lines.Clear();
end;

procedure TMainWindow.dialog_setup();
begin
Self.OpenDialog.Title:='Open a mugen config file';
Self.SaveDialog.Title:='Save a mugen config file';
Self.OpenDialog.Filter:='Mugen config files|*.def;*.cns;*.air;*.cmd;*.cfg';
Self.SaveDialog.Filter:='Game settings|*.def|Configuration file|*.cfg|Character definitive|*.cns|Animation settings|*.air|AI settings|*.cmd';
end;

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
 Self.window_setup();
 Self.interface_setup();
 Self.dialog_setup();
 Self.window_resize();
end;

procedure TMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if Self.OpenDialog.FileName<>'' then
 begin
  Self.Editor.Lines.SaveToFile(Self.OpenDialog.FileName);
 end
 else
 begin
  if Application.MessageBox('Do you want to save an unsaved file?','Save a file',MB_ICONQUESTION+MB_YESNO)=ID_YES then Self.SaveDialog.Execute();
 end;

end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
 Self.window_resize();
end;

procedure TMainWindow.AboutMenuItemClick(Sender: TObject);
begin
 ShowMessage('Mugen config editor. Version 1.8.4. 2007-2026 years. This software was made by Popov Evgeniy Alekseyevich');
end;

procedure TMainWindow.NewMenuItemClick(Sender: TObject);
begin
 Self.OpenDialog.FileName:='';
 Self.Editor.Lines.Clear();
end;

procedure TMainWindow.OpenMenuItemClick(Sender: TObject);
begin
 Self.OpenDialog.Execute();
end;

procedure TMainWindow.SaveMenuItemClick(Sender: TObject);
begin
 if Self.OpenDialog.FileName<>'' then
 begin
  Self.Editor.Lines.SaveToFile(Self.OpenDialog.FileName);
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

procedure TMainWindow.CopyMenuItemClick(Sender: TObject);
begin
 Self.Editor.CopyToClipboard();
end;

procedure TMainWindow.PasteMenuItemClick(Sender: TObject);
begin
 Self.Editor.PasteFromClipboard();
end;

procedure TMainWindow.CutMenuItemClick(Sender: TObject);
begin
 Self.Editor.CutToClipboard();
end;

procedure TMainWindow.OpenDialogCanClose(Sender: TObject; var CanClose: boolean);
begin
 Self.Editor.Lines.LoadFromFile(Self.OpenDialog.FileName);
end;

procedure TMainWindow.SaveDialogCanClose(Sender: TObject; var CanClose: boolean);
begin
 Self.OpenDialog.FileName:=ExtractFileNameWithoutExt(Self.SaveDialog.FileName)+get_file_extension(Self.SaveDialog.FilterIndex);
 Self.Editor.Lines.SaveToFile(Self.OpenDialog.FileName);
end;

end.
