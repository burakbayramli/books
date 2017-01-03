{ Form Template - Source and Destination Choices Lists }
unit Cntry;

{EUGene  Copyright 1997, 1998 D. Scott Bennett Jr. and Allan C. Stam III
 All Rights Reserved}

interface

uses OutWindow, setting, eutypes1, WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  Dialogs, StdCtrls, Printers, SysUtils, StrUtils;

type
  TCountryListDlg = class(TForm)
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    Ok: TBitBtn;
    Cancel: TBitBtn;
    Help: TBitBtn;
    procedure HelpClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
    procedure DualListDlg(Sender: TObject);
    procedure OkClick(Sender: TObject);
  private
     { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses PagedOutput;

{$R *.DFM}
{------------------------------------------------------------------------}

procedure TCountryListDlg.HelpClick(Sender: TObject);
begin
     ShowMessage('Choose Countries to include and click on Ok.');
end;

{-----------------------------------------------------------------------}

procedure TCountryListDlg.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList.Items);
  SetItem(SrcList, Index);
end;

{-----------------------------------------------------------------------}

procedure TCountryListDlg.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList.Items);
  SetItem(DstList, Index);
end;

{------------------------------------------------------------------------}

procedure TCountryListDlg.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I],
      SrcList.Items.Objects[I]);
  SrcList.Items.Clear;
  SetItem(SrcList, 0);
end;

{-----------------------------------------------------------------------}

procedure TCountryListDlg.ExcAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to DstList.Items.Count - 1 do
    SrcList.Items.AddObject(DstList.Items[I], DstList.Items.Objects[I]);
  DstList.Items.Clear;
  SetItem(DstList, 0);
end;

{----------------------------------------------------------------------}

procedure TCountryListDlg.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count - 1 downto 0 do
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
end;

{-------------------------------------------------------------------------}

procedure TCountryListDlg.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

{---------------------------------------------------------------------------}

function TCountryListDlg.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

{-------------------------------------------------------------------------}

procedure TCountryListDlg.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
       else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;

{-------------------------------------------------------------------------}

procedure TCountryListDlg.DualListDlg(Sender: TObject);
var
  ccode, x: Integer;
  nameandcode, selected_name: string;
begin
  SrcList.items.clear;
  for ccode := min_ccode to max_ccode do
    if nation_list.have_info(ccode) then
    begin
      nameandcode := nation_list.get_fullname(ccode) + ' (' + inttostr(ccode) + ')';
      SrcList.items.add(nameandcode);
    end;
  DstList.items.clear;
  for x := 1 to length(user_selections.selected_country_list.data) do
     begin
        if user_selections.selected_country_list.data[x] <> 0 then
        begin
           selected_name := nation_list.get_fullname(user_selections.selected_country_list.data[x]);
           nameandcode := selected_name + ' (' + inttostr(user_selections.selected_country_list.data[x]) + ')';
           DstList.items.Add(nameandcode);
        end;

     end;

 end;

{-----------------------------------------------------------------------}

procedure TCountryListDlg.OkClick(Sender: TObject);
var x : integer;
    nameandnocode: string;
begin
     SettingsForm.ListBox1.items:=DstList.Items;
     SettingsForm.ListBox1.show;
     user_selections.selected_country_list.num_countries:=0;

     {need to check format for accessing dstlist here, and need to convert list names to ccodes.}
     for x := 0 to dstList.Items.count-1 do
        begin
           inc(user_selections.selected_country_list.num_countries);
           nameandnocode := LeftStr(DstList.items[x], Pos('(', DstList.items[x]) - 2);
           user_selections.selected_country_list.data[user_selections.selected_country_list.num_countries] :=
              nation_list.get_ccode_from_fullname(nameandnocode);
        end;

     if (user_selections.output_this = output_directed_dyads) or (user_selections.output_this = output_nondirected_dyads) then
       begin
         if (DstList.Items.count >= 2) then
         begin
           user_selections.dyads_selected := selected_set;
           close;
         end
         else ShowMessage ('You must select at least 2 countries');
       end
     else if (user_selections.output_this = output_monads) then
       begin
         if (DstList.Items.count >= 1) then
         begin
           user_selections.monads_selected := selected_set_mono;
           close;
         end
         else ShowMessage ('You must select at least 1 country');
       end
     else showmessage ('Error - program values undefined in selected set click');

           {if user properly selected countries, then change cntrylabel box and set radiodial.}
   if (user_selections.dyads_selected = selected_set) or
      (user_selections.monads_selected = selected_set_mono) then
      begin
        SettingsForm.cntryLabel.caption:='Countries Selected = User-Specified Subset';
        modalresult := MROK;
      end;

  end;

{-----------------------------------------------------------------------------}

procedure TCountryListDlg.CancelClick(Sender: TObject);
begin
   modalresult := mrNone;
   Close;
end;


end.
