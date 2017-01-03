unit VarBrowsePopup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus;

type
  TPopupBrowseForm = class(TForm)
    PopupMenu1: TPopupMenu;
    BrowseData1: TMenuItem;
    SelectVariable1: TMenuItem;
    procedure BrowseData1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PopupBrowseForm: TPopupBrowseForm;

implementation

uses browser;

{$R *.dfm}

procedure TPopupBrowseForm.BrowseData1Click(Sender: TObject);
begin
   DatasetBrowserForm.showmodal;
end;

end.
