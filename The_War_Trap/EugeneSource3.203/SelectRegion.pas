unit SelectRegion;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Eutypes1, StdCtrls, Buttons, ExtCtrls;

type
  TRegionSelectForm = class(TForm)
    OKBtn: TBitBtn;
    RegionHelpBtn: TBitBtn;
    SelectRegionPanel: TPanel;
    SelectRegionLabel: TLabel;
    EuropeBox: TCheckBox;
    MideastBox: TCheckBox;
    AfricaBox: TCheckBox;
    AsiaBox: TCheckBox;
    AmericasBox: TCheckBox;
    GlobeBox: TCheckBox;
    procedure RegionHelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RegionChoiceClick(Sender: TObject);
    procedure GlobalChoiceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RegionSelectForm: TRegionSelectForm;

implementation

{$R *.dfm}



procedure TRegionSelectForm.RegionHelpBtnClick(Sender: TObject);
begin
   ShowMessage ('Select the regions you want to draw from for your output data file.  '
               +'If generating dyadic data, only dyads contained entirely within the selected regions will appear.  '
               +'If you do not want to limit your data geographically, leave the default selection intact,'
               +'or select a different option on the previous screen.');
end;


procedure TRegionSelectForm.OKBtnClick(Sender: TObject);
begin
   with user_selections do
   begin
      if EuropeBox.checked then
      begin
         include(selected_regions, europe);
         exclude(selected_regions, globe);
      end;
      if not EuropeBox.checked then exclude(selected_regions, europe);
      if MideastBox.checked then
      begin
         include(selected_regions, middleeast);
         exclude(selected_regions, globe);
      end;
      if not MideastBox.checked then exclude(selected_regions, middleeast);
      if AfricaBox.checked then
      begin
         include(selected_regions, africa);
         exclude(selected_regions, globe);
      end;
      if not AfricaBox.checked then exclude(selected_regions, africa);
      if AsiaBox.checked then
      begin
         include(selected_regions, asia);
         exclude(selected_regions, globe);
      end;
      if not AsiaBox.checked then exclude(selected_regions, asia);
      if AmericasBox.checked then
      begin
         include(selected_regions, americas);
         exclude(selected_regions, globe);
      end;
      if not AmericasBox.checked then exclude(selected_regions, americas);
      if GlobeBox.checked then
      begin
         include(selected_regions, globe);
         exclude(selected_regions, europe);
         exclude(selected_regions, middleeast);
         exclude(selected_regions, africa);
         exclude(selected_regions, asia);
         exclude(selected_regions, americas);
      end;
   end;
end;

procedure TRegionSelectForm.RegionChoiceClick(Sender: TObject);
begin
   if (EuropeBox.checked) or (MideastBox.checked) or (AfricaBox.checked)
      or (AsiaBox.checked) or (AmericasBox.checked) then
      GlobeBox.checked := false;
end;

procedure TRegionSelectForm.GlobalChoiceClick(Sender: TObject);
begin
   if GlobeBox.checked then
   begin
      EuropeBox.checked := false;
      MideastBox.checked := false;
      AfricaBox.checked := false;
      AsiaBox.checked := false;
      AmericasBox.checked := false;
      GlobeBox.checked := true;
   end;
end;

procedure TRegionSelectForm.FormShow(Sender: TObject);
begin
   EuropeBox.checked := (europe in user_selections.selected_regions);
   MideastBox.checked := (middleeast in user_selections.selected_regions);
   AfricaBox.checked := (africa in user_selections.selected_regions);
   AsiaBox.checked := (asia in user_selections.selected_regions);
   AmericasBox.checked := (americas in user_selections.selected_regions);
   GlobeBox.checked := (globe in user_selections.selected_regions);
end;


end.
