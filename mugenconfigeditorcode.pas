unit mugenconfigeditorcode;

{$mode objfpc}{$H+}

interface

uses
  Classes,LCLProc,LCLType,SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TForm1 }

    TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure OpenDialog1CanClose(Sender: TObject; var CanClose: boolean);
    procedure SaveDialog1CanClose(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure window_setup();
procedure window_resize();
procedure interface_setup();
procedure dialog_setup();
function get_file_extension(filter:Byte):string;
function check_format(name:string):boolean;
procedure load_mugen_config();
var Form1: TForm1;

implementation

procedure window_setup();
begin
 Application.Title:='MUGEN CONFIG EDITOR';
 Form1.Caption:='MUGEN CONFIG EDITOR';
 Form1.Font.Name:=Screen.MenuFont.Name;
 Form1.Font.Size:=14;
end;

procedure window_resize();
begin
Form1.Memo1.Width:=Form1.ClientWidth-10;
Form1.Memo1.Height:=Form1.ClientHeight-10;
end;

procedure interface_setup();
begin
Form1.OpenDialog1.FileName:='';
Form1.Memo1.ScrollBars:=ssBoth;
Form1.MainMenu1.Items.Items[0].Items[0].ShortCut:=TextToShortCut('Ctrl+N');
Form1.MainMenu1.Items.Items[0].Items[1].ShortCut:=TextToShortCut('Ctrl+O');
Form1.MainMenu1.Items.Items[0].Items[2].ShortCut:=TextToShortCut('Ctrl+S');
Form1.MainMenu1.Items.Items[0].Items[3].ShortCut:=TextToShortCut('Ctrl+Alt+S');
Form1.MainMenu1.Items.Items[1].Items[0].ShortCut:=TextToShortCut('Ctrl+C');
Form1.MainMenu1.Items.Items[1].Items[1].ShortCut:=TextToShortCut('Ctrl+V');
Form1.MainMenu1.Items.Items[1].Items[2].ShortCut:=TextToShortCut('Ctrl+X');
Form1.Memo1.Lines.Clear();
end;

procedure dialog_setup();
begin
Form1.OpenDialog1.Title:='Open a mugen config file';
Form1.SaveDialog1.Title:='Save a mugen config file';
Form1.OpenDialog1.Filter:='Mugen config files|*.def;*.cns;*.air;*.cmd;*.cfg';
Form1.SaveDialog1.Filter:='Game setting|*.def|Configuration file|*.cfg|Character definitive|*.cns|Animation setting|*.air|AI setting|*.cmd';
end;

function get_file_extension(filter:Byte):string;
var extension:array[1..5] of string=('.def','.cfg','.cns','.air','.cmd');
begin
get_file_extension:=extension[filter];
end;

function check_format(name:string):boolean;
var extension:string;
var filter:Byte;
var status:boolean;
begin
status:=false;
extension:=ExtractFileExt(name);
for filter:=1 to 5 do
begin
if extension=get_file_extension(filter) then
begin
status:=true;
break;
end;

end;
check_format:=status;
end;

procedure load_mugen_config();
begin
if check_format(Form1.OpenDialog1.FileName)=false then
begin
ShowMessage('Unsupported file type');
Form1.OpenDialog1.FileName:='';
end
else
begin
Form1.Memo1.Lines.LoadFromFile(Form1.OpenDialog1.FileName);
end;

end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
window_setup();
interface_setup();
dialog_setup();
window_resize();
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
if Form1.OpenDialog1.FileName<>'' then
begin
Form1.Memo1.Lines.SaveToFile(Form1.OpenDialog1.FileName);
end
else
begin
if Application.MessageBox('Do you want to save unsaved file?','Save a file',MB_ICONQUESTION+MB_YESNO)=ID_YES then Form1.SaveDialog1.Execute();
end;

end;

procedure TForm1.FormResize(Sender: TObject);
begin
window_resize();
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
ShowMessage('Mugen config editor.Version 1.7.4. 2007-2018 years.This software made by Popov Evgeniy Alekseyevich');
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
Form1.OpenDialog1.FileName:='';
Form1.Memo1.Lines.Clear();
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
Form1.OpenDialog1.Execute();
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
if Form1.OpenDialog1.FileName<>'' then
begin
Form1.Memo1.Lines.SaveToFile(Form1.OpenDialog1.FileName);
end
else
begin
Form1.SaveDialog1.Execute();
end;

end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
Form1.SaveDialog1.Execute();
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
Form1.Memo1.CopyToClipboard();
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
Form1.Memo1.PasteFromClipboard();
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
begin
Form1.Memo1.CutToClipboard();
end;

procedure TForm1.OpenDialog1CanClose(Sender: TObject; var CanClose: boolean);
begin
Form1.Memo1.Lines.LoadFromFile(Form1.OpenDialog1.FileName);
end;

procedure TForm1.SaveDialog1CanClose(Sender: TObject; var CanClose: boolean);
begin
Form1.OpenDialog1.FileName:=ExtractFileNameWithoutExt(Form1.SaveDialog1.FileName)+get_file_extension(Form1.SaveDialog1.FilterIndex);
Form1.Memo1.Lines.SaveToFile(Form1.OpenDialog1.FileName);
end;

end.
