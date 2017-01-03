unit browser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mdiframe, eutypes1, Grids, StdCtrls, ComCtrls;

type
  TDatasetBrowserForm = class(TForm)
    DatasetGrid: TStringGrid;
    FileSelectionBox: TComboBox;
    LoadButton: TButton;
    SearchField: TEdit;
    SearchButton: TButton;
    ExitButton: TButton;
    FixColumnButton: TButton;
    ProgressBar1: TProgressBar;
    PercentageLabel: TLabel;
    PercentLabel: TLabel;
    procedure LoadButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FixColumnButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);

  private
    { Private declarations }
    config : configuration_type;
    DisplayFileName : string;
    BrowserFileNameIndex : TStrings;
    liInitialMemory : longint;
    liFinalMemory : longint;
    procedure BuildData;
    procedure TestDataSize;

  public
    { Public declarations }
    procedure Init(configuration : configuration_type; Sender : TObject);


  end;

implementation

{$R *.dfm}
procedure TDatasetBrowserForm.Init(configuration : configuration_type; Sender : TObject);
var
  sBrowserIniPath : string;
  tfBrowserIni : textfile;
  chTemp : char;
  iStringEntry : integer;
  strTemp : String;
  bComment : bool;
  ctr : integer;

begin
    

    config := configuration;


    BrowserFileNameIndex := Tstringlist.create;

    BrowserFileNameIndex.NameValueSeparator := '=';

    iStringEntry := 0;

    ProgressBar1.Visible := false;
    PercentageLabel.Visible := false;

    sBrowserIniPath := config.browser_lookup_file;

    AssignFile(tfBrowserIni, sBrowserIniPath);
    Reset(tfBrowserIni);



    while(Eof(tfBrowserIni) = false) do
      begin
        Read(tfBrowserIni, chTemp);

        {if line is a comment line, skip through it}
        if(chTemp = ';') then
          begin
            bComment := true;
            while(Eoln(tfBrowserIni) = false) do
              Read(tfBrowserIni, chTemp);
          end
        else
          bComment := false;

        if(chTemp <> ';') and {(Eoln(tfBrowserIni) = false) and} (bComment = false) then
          begin
            {BrowserFileNameIndex[iStringEntry] := BrowserFileNameIndex[iStringEntry] + chTemp;   }
            {StrCat(strTemp, chTemp);}
            strTemp := strTemp + chTemp;
          end;


        if(Eoln(tfBrowserIni) = true) then
          begin
            if bComment = false then
              begin
                {BrowserFileNameIndex.Count := BrowserFileNameIndex.Count + 1;    }
                {BrowserFileNameIndex.Insert(iStringEntry, strTemp);}
                {whereimat := BrowserFileNameIndex.add(strTemp);}
                with BrowserFileNameIndex do
                      add (strtemp);
                iStringEntry := iStringEntry + 1;
                strTemp := '';
                
              end;
            Read(tfBrowserIni, chTemp);
            Read(tfBrowserIni, chTemp);

          end;
      end;

    ctr := 0;
    
    While (ctr < iStringEntry) do
      begin
        FileSelectionBox.Items.Add(BrowserFileNameIndex.Names[ctr]);
        ctr := ctr + 1;
      end;
    {FileSelectionBox.Items.AddStrings(BrowserFileNameIndex);}

    CloseFile(tfBrowserIni);

    ctr := 0;

end;


procedure TDatasetBrowserForm.BuildData;
var
  tfDisplayFile : textfile;
  i, j : integer;
  chTemp : Char;
  bFirstLine : bool;
  chSeparator : Char;
  bQuote : bool;
  iCtr, iFileSize : integer;
  rProgressLevel : real;
  rTest : real;

begin
    {Load the file into the grid}

    i := 0;
    j := 0;
    iCtr := 0;
    rProgressLevel := 0.1;
    rTest := 0;

    liInitialMemory := availphysicalmem();
    AssignFile(tfDisplayFile, DisplayFileName);
    FileMode := fmOpenRead;
    Reset(tfDisplayFile);

    iFileSize := FileSize(tfDisplayFile) * 128;
    ProgressBar1.Visible := true;
    PercentageLabel.Caption := floattostr(rTest);
    PercentageLabel.Visible := true;
    PercentLabel.Visible := true;
    PercentLabel.refresh;



    bFirstLine := true;
    bQuote := false;
    ProgressBar1.Stepit;

    while(Eof(tfDisplayFile) = false) do
      begin
        Read(tfDisplayFile, chTemp);
        iCtr := iCtr + 1;

        rTest := (iCtr )/iFileSize;
        if ((iCtr)/iFileSize) > rProgressLevel then
          begin
             ProgressBar1.StepIt;
             rTest := (rTest * 100);

             PercentageLabel.Caption := floattostr(rTest);
             PercentageLabel.refresh;
             PercentLabel.refresh;


             rProgressLevel := rProgressLevel + 0.1;
          end;

        if(bFirstLine = false) then
          begin

            if(chTemp = '"') then
              begin
                if (bQuote = true) then
                  bQuote := false
                else
                  bQuote := true;
              end;

            if(chTemp <> chSeparator) and (chTemp <> '"') then
              begin
                DatasetGrid.Cells[i,j] := Datasetgrid.Cells[i,j] + chTemp;
              end;

            if(chTemp = chSeparator) and (bQuote = false) then
              begin
                i := i + 1;
                if (j = 0) then
                  DatasetGrid.ColCount := i+1;
              end;

            if(eoln(tfDisplayFile) = true) then
              begin
                j := j + 1;
                i := 0;

                if(eof(tfDisplayFile) <> true) then
                  DataSetGrid.RowCount := j+1;

                Read(tfDisplayFile, chTemp);
                Read(tfDisplayFile, chTemp);
              end;
          end;

        if(bFirstLine = true) then
          begin
            if (chTemp <> ',') {and (chTemp <> ' ') }and (chTemp <> chr(9)) then
              DatasetGrid.Cells[i,j] := Datasetgrid.Cells[i,j] + chTemp;

            if (chTemp = ',') then
              begin
                chSeparator := chTemp;
                i := i + 1;
                if (j = 0) then
                  DatasetGrid.ColCount := i+1;
              end;

            {if (chTemp = ' ') then
              begin
                chSeparator := chTemp;
                i := i + 1;
                if (j = 0) then
                  DatasetGrid.ColCount := i+1;
              end;     }

            if (chTemp = chr(9)) then
              begin
                chSeparator := chTemp;
                i := i + 1;
                if (j = 0) then
                  DatasetGrid.ColCount := i+1;
              end;

            if(eoln(tfDisplayFile) = true) then
              begin
                j := j + 1;
                i := 0;
                bFirstLine := false;

                if(eof(tfDisplayFile) <> true) then
                  DataSetGrid.RowCount := j+1;

                Read(tfDisplayFile, chTemp);
                Read(tfDisplayFile, chTemp);
              end;
          end;

        end;
    liFinalMemory := availphysicalmem();
    {TestDataSize;     }
    CloseFile(tfDisplayFile);
    DatasetGrid.FixedRows := 1;
    ProgressBar1.Visible := false;
    PercentageLabel.Visible := False;
    PercentLabel.Visible := false;


 
end;



procedure TDatasetBrowserForm.LoadButtonClick(Sender: TObject);
var
  strTemp : String;
  FilePath : String;
  chTemp : char;
  ctr, i, j, i_max, j_max : integer;
  bPath : bool;
  liFileMem : longint;
  liFileSizeAdjusted : longint;
  fTest : file;
  bFileOk : bool;

begin

   ctr := 1;
   bPath := false;
   bFileOk := true;

   if(FileSelectionBox.ItemIndex = -1) then
    bFileOk := false;

   if(bFileOk = false) then
    ShowMessage('Invalid selection, select a file from the list.');

   if(bFileOk = true) then
    begin
      strTemp := BrowserFileNameIndex[FileSelectionBox.ItemIndex];
      While ((ctr - 1) < StrLen(PChar(strTemp))) do
        begin
          chTemp := strTemp[ctr];
          if (bPath = true) then
            FilePath := FilePath + chTemp;
          if (chTemp = '=') then
            bPath := true;
          ctr := ctr + 1;
        end;


      DisplayFileName := config.eugene_directory + FilePath;

      if (FileExists(DisplayFileName)) then
      begin

        
      i := 0;
      i_max := DatasetGrid.ColCount;
      j := 0;
      j_max := DatasetGrid.RowCount;

      for i := 0 to i_max do
        Datasetgrid.Cols[i].clear;
      for j := 0 to j_max do
        Datasetgrid.rows[j].clear;

      DataSetGrid.RowCount := 0;
      DataSetGrid.ColCount := 0;

        AssignFile(fTest, DisplayFileName);
        Reset(fTest, 1);
        {liFileMem := FileSize(fTest);}
        liFileMem := availphysicalmem();
        liFileSizeAdjusted := FileSize(fTest) * 20;
        {showmessage(inttostr(liFileMem));}
        if(liFileSizeAdjusted > (liFileMem * (1/10))) then
          begin
            showmessage('Can not open file, too few system resources.')
          end
        else
          begin
            CloseFile(fTest);
            BuildData;
          end;
      end
      else
      begin
        ShowMessage('File does not exist, cannot be opened.');
        FileSelectionBox.Items.Delete(FileSelectionBox.ItemIndex);
      end;

    end;
end;

procedure TDatasetBrowserForm.TestDataSize;
var
  liSize : longint;
  i,j : integer;

begin

  {for i := 0 to datasetgrid.ColCount do
    for j := 0 to datasetgrid.RowCount do
      liSize := liSize + Sizeof(datasetgrid.Cells[i,j]);

  for i := 0 to datasetgrid.ColCount do
    liSize := liSize + sizeof(datasetgrid.Cols[i]);

  for j := 0 to datasetgrid.RowCount do
    liSize := liSize + sizeof(datasetgrid.Rows[i]);  }
       liSize := liInitialMemory - liFinalMemory;
  {showmessage(inttostr(availphysicalmem()));       }
  showmessage('Estimated Size: '+inttostr(liSize)+' Initial Mem: '+inttostr(liInitialMemory)+' Final Mem: '+inttostr(liFinalMemory));

end;

procedure TDatasetBrowserForm.SearchButtonClick(Sender: TObject);
var
  sSearchText : String;
  iInitialRow : integer;
  i,j : integer;
  bFound : bool;
begin
  {search for user defined value}
  sSearchText := SearchField.Text;

  iInitialRow := DatasetGrid.Row + 1;
  bFound := false;

  for i := 0 to datasetgrid.colCount do
    for j := iInitialRow to datasetgrid.rowCount do
      if (strcomp(pchar(sSearchText), pchar(datasetgrid.cells[i,j])) = 0) then
        begin
          datasetgrid.Row := j;
          bFound := true;

         break;
        end;
  if bFound = false then
    Showmessage('Value not found.');

end;


procedure TDatasetBrowserForm.ExitButtonClick(Sender: TObject);
var
  i,j : integer;
begin
  {Exit procedure}
  Close;

end;

procedure TDatasetBrowserForm.FixColumnButtonClick(Sender: TObject);
begin
  if(DatasetGrid.FixedCols = 0) then
    begin
      FixColumnButton.Caption := 'Free First Column';
      DatasetGrid.FixedCols := 1;
    end
  else
    begin
      FixColumnButton.Caption := 'Fix First Column';
      DatasetGrid.FixedCols := 0;
    end;
end;

procedure TDatasetBrowserForm.FormResize(Sender: TObject);
begin
  SearchButton.Left := self.Width - 140;
  SearchField.Left := self.Width - 292;
  ExitButton.Left := self.Width - 170;
  FixColumnButton.Top := self.Height - 60;
  ExitButton.Top := self.Height - 60;
  ProgressBar1.Top := self.Height - 50;

end;




end.
