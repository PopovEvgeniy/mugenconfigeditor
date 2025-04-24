unit mugenconfigeditorcode;

{$mode objfpc}{$H+}

interface

uses
  Classes,LCLProc,LCLType,SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, LazFileUtils;

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
    { private declarations }
  public
    { public declarations }
  end;

var MainWindow: TMainWindow;

implementation

procedure window_setup();
begin
 Application.Title:='Mugen config editor';
 MainWindow.Caption:=Application.Title;
 MainWindow.Font.Name:=Screen.MenuFont.Name;
 MainWindow.Font.Size:=14;
end;

procedure window_resize();
begin
 MainWindow.Editor.Width:=MainWindow.ClientWidth-10;
 MainWindow.Editor.Height:=MainWindow.ClientHeight-10;
end;

procedure interface_setup();
begin
 MainWindow.OpenDialog.FileName:='';
 MainWindow.Editor.ScrollBars:=ssBoth;
 MainWindow.MainMenu.Items.Items[0].Items[0].ShortCut:=TextToShortCut('Ctrl+N');
 MainWindow.MainMenu.Items.Items[0].Items[1].ShortCut:=TextToShortCut('Ctrl+O');
 MainWindow.MainMenu.Items.Items[0].Items[2].ShortCut:=TextToShortCut('Ctrl+S');
 MainWindow.MainMenu.Items.Items[0].Items[3].ShortCut:=TextToShortCut('Ctrl+Alt+S');
 MainWindow.MainMenu.Items.Items[1].Items[0].ShortCut:=TextToShortCut('Ctrl+C');
 MainWindow.MainMenu.Items.Items[1].Items[1].ShortCut:=TextToShortCut('Ctrl+V');
 MainWindow.MainMenu.Items.Items[1].Items[2].ShortCut:=TextToShortCut('Ctrl+X');
 MainWindow.Editor.Lines.Clear();
end;

procedure dialog_setup();
begin
MainWindow.OpenDialog.Title:='Open a mugen config file';
MainWindow.SaveDialog.Title:='Save a mugen config file';
MainWindow.OpenDialog.Filter:='Mugen config files|*.def;*.cns;*.air;*.cmd;*.cfg';
MainWindow.SaveDialog.Filter:='Game settings|*.def|Configuration file|*.cfg|Character definitive|*.cns|Animation settings|*.air|AI settings|*.cmd';
end;

function get_file_extension(const filter:Byte):string;
var extension:array[1..5] of string=('.def','.cfg','.cns','.air','.cmd');
begin
 get_file_extension:=extension[filter];
end;

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
 window_setup();
 interface_setup();
 dialog_setup();
 window_resize();
end;

procedure TMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if MainWindow.OpenDialog.FileName<>'' then
 begin
  MainWindow.Editor.Lines.SaveToFile(MainWindow.OpenDialog.FileName);
 end
 else
 begin
  if Application.MessageBox('Do you want to save an unsaved file?','Save a file',MB_ICONQUESTION+MB_YESNO)=ID_YES then MainWindow.SaveDialog.Execute();
 end;

end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
 window_resize();
end;

procedure TMainWindow.AboutMenuItemClick(Sender: TObject);
begin
 ShowMessage('Mugen config editor. Version 1.8.1. 2007-2025 years.This software was made by Popov Evgeniy Alekseyevich');
end;

procedure TMainWindow.NewMenuItemClick(Sender: TObject);
begin
 MainWindow.OpenDialog.FileName:='';
 MainWindow.Editor.Lines.Clear();
end;

procedure TMainWindow.OpenMenuItemClick(Sender: TObject);
begin
 MainWindow.OpenDialog.Execute();
end;

procedure TMainWindow.SaveMenuItemClick(Sender: TObject);
begin
 if MainWindow.OpenDialog.FileName<>'' then
 begin
  MainWindow.Editor.Lines.SaveToFile(MainWindow.OpenDialog.FileName);
 end
 else
 begin
  MainWindow.SaveDialog.Execute();
 end;

end;

procedure TMainWindow.SaveAsMenuItemClick(Sender: TObject);
begin
 MainWindow.SaveDialog.Execute();
end;

procedure TMainWindow.CopyMenuItemClick(Sender: TObject);
begin
 MainWindow.Editor.CopyToClipboard();
end;

procedure TMainWindow.PasteMenuItemClick(Sender: TObject);
begin
 MainWindow.Editor.PasteFromClipboard();
end;

procedure TMainWindow.CutMenuItemClick(Sender: TObject);
begin
 MainWindow.Editor.CutToClipboard();
end;

procedure TMainWindow.OpenDialogCanClose(Sender: TObject; var CanClose: boolean);
begin
 MainWindow.Editor.Lines.LoadFromFile(MainWindow.OpenDialog.FileName);
end;

procedure TMainWindow.SaveDialogCanClose(Sender: TObject; var CanClose: boolean);
begin
 MainWindow.OpenDialog.FileName:=ExtractFileNameWithoutExt(MainWindow.SaveDialog.FileName)+get_file_extension(MainWindow.SaveDialog.FilterIndex);
 MainWindow.Editor.Lines.SaveToFile(MainWindow.OpenDialog.FileName);
end;

end.
