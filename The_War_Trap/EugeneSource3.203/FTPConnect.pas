unit FTPConnect;

{Unit to connect to EUGene ftp site, download file list, and allow user
 upload/download of additional data sets.
 Procedured works by downloading a description file of user data sets from the
 server.  This file lists the component files associated with each user data set,
 so that EUGene client knows what files to download.}

{NOTE that on the server side, the server must be set to use relative paths
 to the login directory for the code to not generate an error (as the code
 changes directory to "\").  On Serv-U, this means that the setting "Lock User in Home Directory"
 must be true.

{This unit uses and modifies code originally part of the INDY ftp components demo
 file,  Project  : FTPDemo,  Unit Name: mainf,  Version  : 1.0 dated Wed 25 Apr 2001.
 Author of FTPDemo was Doychin Bondzhev <doichin@5group.com>.  }

{INDY copyright notice:
 Portions of this software are Copyright (c) 1993 - 2001, Chad Z. Hower (Kudzu) and
 the Indy Pit Crew - http://www.nevrona.com/Indy/

 THIS SOFTWARE IS PROVIDED BY Chad Z. Hower (Kudzu) and the Indy Pit Crew "AS IS''
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 SUCH DAMAGE.   }


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Grids, Mask, EUTypes1, cmnprocd,
  FileCtrl, Buttons, Outline, ExtCtrls, httpapp, strutils, getfilesforupload,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdFTP, IdException,
  IdExplicitTLSClientServerBase;

type

  TFTP_Form = class(TForm)
    EmailEdit: TEdit;
    emailLabel: TLabel;
    ConnectButton: TButton;
    Memo1: TMemo;
    UploadButton: TButton;
    DownloadButton: TButton;
    CloseButton: TBitBtn;
    O: TOpenDialog;
    ServerFileBox: TGroupBox;
    StringGrid1: TStringGrid;
    SaveDialog1: TSaveDialog;
    HelpButton: TBitBtn;
    CancelAbortButton: TBitBtn;
    IdFTP1: TIdFTP;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    FTPSiteLabel: TLabel;
    ftpedit: TEdit;
    UsePassive: TCheckBox;
    ProgressBar1: TProgressBar;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UploadButtonClick(Sender: TObject);
    procedure DownloadButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure updatestringgrid;
    procedure FormDestroy(Sender: TObject);
    procedure EmailEditChange(Sender: TObject);
    {procedure enable_all_buttons; }
    procedure disable_most_buttons;
    {procedure enable_most_buttons;
    procedure enable_exception_buttons; }
    procedure HelpButtonClick(Sender: TObject);
    procedure CancelAbortButtonClick(Sender: TObject);
    procedure IdFTPOnConnected(Sender: TObject);
    procedure IdFTP1OnDisconnected(Sender: TObject);
    procedure UsePassiveClick(Sender: TObject);
    procedure IdFTP1OnWork(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
    procedure IdFTP1OnWorkBegin(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCountMax: Integer);
    procedure IdFTP1OnWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
    procedure IdFTP1Status(axSender: TObject; const axStatus: TIdStatus;
      const asStatusText: String);
  private
    AbortTransfer: Boolean;
    TransferringData : boolean;   {marks if in middle of transfer for abort}
    BytesToTransfer: LongWord;
    BytesRecvd, BytesSent : longWord;
    AverageSpeed : real;
    STime: TDateTime;
    SelectedForDownload : boolean;
    procedure disconnect;
    procedure eidexception_handle (var handled : boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

  extension_rec_type = record
        data_seen : boolean;
        edf_seen : boolean;
        doc_seen : boolean;
     end;
  extension_data_type = array of extension_rec_type;

  transfer_type = (upload, download);

  User_data_file_list_type = array of record
        datasetname : string;
        datasets : array of string;
     end;


var
  FTP_Form: TFTP_Form;
  UploadFilenames : Tstringlist;
  WriteableLocation : string;   {path where temporary files can be saved}
  ServerUserDataSets : User_data_file_list_type;
  handled : boolean;

implementation

uses IdFTPCommon;
{$R *.DFM}


{  ---------------------------------------------------------------------  }
{  Basic Form functions here }

{procedure TFTP_Form.enable_all_buttons;
   begin
     FTP_Form.connectButton.enabled := true;
     FTP_Form.DownloadButton.enabled := true;
     FTP_Form.UploadButton.enabled := true;
     FTP_Form.CloseButton.enabled := true;
     FTP_Form.HelpButton.enabled := true;
     FTP_Form.CancelAbortButton.enabled := false;
     FTP_Form.CancelAbortButton.visible := false;
   end;

procedure TFTP_Form.enable_exception_buttons;
   begin
     FTP_Form.connectButton.enabled := true;
     FTP_Form.DownloadButton.enabled := false;
     FTP_Form.UploadButton.enabled := false;
     FTP_Form.CloseButton.enabled := true;
     FTP_Form.HelpButton.enabled := true;
     FTP_Form.CancelAbortButton.enabled := true;
     FTP_Form.CancelAbortButton.visible := true;
   end;
}
procedure TFTP_Form.disable_most_buttons;
   begin
     FTP_Form.connectButton.enabled := false;
     FTP_Form.DownloadButton.enabled := false;
     FTP_Form.UploadButton.enabled := false;
     FTP_Form.CloseButton.enabled := true;
     FTP_Form.HelpButton.enabled := true;
     FTP_Form.CancelAbortButton.enabled := false;
     FTP_Form.CancelAbortButton.visible := false;
   end;

{procedure TFTP_Form.enable_most_buttons;
   begin
     FTP_Form.connectButton.enabled := true;
     FTP_Form.DownloadButton.enabled := false;
     FTP_Form.UploadButton.enabled := false;
     FTP_Form.CloseButton.enabled := true;
     FTP_Form.HelpButton.enabled := true;
     FTP_Form.CancelAbortButton.enabled := false;
     FTP_Form.CancelAbortButton.visible := false;
   end;
}

function OK_email (address : string) : boolean;
   begin
      OK_email := true;
      if (address = '') or (address = 'myemail@somewhere.edu') or (AnsiContainsText(address, 'xyz123'))
          or (AnsiContainsText(address, 'anonymous')) or (AnsiContainsText(address, 'asdf'))
          or (AnsiContainsText(address, 'abc123')) then OK_email := false;
      {must have @ and . after}
      if not (
         (pos ('@', address) > 1) and
         (pos ('.',
               copy(address, pos('@', address), length(address)-pos('@', address))) > 1)  )
         then OK_email := false;
   end;

procedure TFTP_Form.updatestringgrid;
var
   I, num_good_files, current_row : Integer;
   row, column : integer;
   begin
      with stringgrid1 do
         begin
            {rowcount := length(ServerUserDataSets)+1;}
            rowcount := length(ServerUserDataSets);
            {if rowcount < 2 then
               begin
                  rowcount := 2;
                  rows[1].clear;
               end;}
            {fixedrows := 1;}

            if  length(ServerUserDataSets) > 0 then
              begin
                 num_good_files := 0;
                 {current_row := 1;   {will put entry on this row}
                 current_row := 0;   {will put entry on this row}
                 for I := 0 to ( length(ServerUserDataSets) - 1) do
                    begin
                       Cells[0, current_row] := inttostr(current_row+1)+'.  '+ ServerUserDataSets[I].datasetname;
                       inc(current_row);
                       inc(num_good_files);
                    end;
                 StringGrid1.RowCount := num_good_files;
                 {StringGrid1.RowCount := num_good_files+1;}
              end;
         end;   {with stringgrid1}

   end;

procedure TFTP_Form.StringGrid1SelectCell(Sender: TObject; ACol,
   ARow: Integer; var CanSelect: Boolean);
   begin
      {if Arow > 0 then}
      if Arow >= 0 then
         begin
            canselect := true;
            if (Arow >=0) and  (IDFTP1.Connected) then
               begin
                  DownloadButton.enabled := true;
                  SelectedForDownload := true;
               end;
         end
      else
         canselect := false;
   end;

procedure TFTP_Form.FormCreate(Sender: TObject);
var
  myRect: TGridRect;
  emailfile : textfile;
  emailstring : string;
begin
   ProgressBar1.Parent := StatusBar1;
   ProgressBar1.Top := 2;
   ProgressBar1.Left := 1;
   ProgressBar1.width := StatusBar1.panels[0].width-2;
   ProgressBar1.hide;
   StatusBar1.Panels[0].Text := 'Disconnected';
   StatusBar1.Panels[1].Text := 'Not Connected';

   ftp_form.width := serverfilebox.width + 40;
   ftp_form.height := Panel1.height + serverfilebox.height + downloadbutton.height +
      memo1.height + closebutton.height + StatusBar1.height + 80;

   {create my own storage list object here, because I have to
    parse the ftp file lines myself.}
   {ftpfilesname := TStringList.Create;
   modifdate := TStringList.Create;
   size := TStringList.Create;
   attribute := TStringList.Create;}

   UploadFileNames := Tstringlist.create;

   {LocalDir := configuration.user_data_files_directory;}
   With StringGrid1 do
      begin
         RowCount := 1;
         {RowCount := 2;
         fixedrows := 1;  }
         ColCount := 1;        {was 3}
         fixedcols := 0;
         Cells[0, 0] := '  Not connected';
         {Cells[0, 0] := 'Filename';
         Cells[1, 0] := 'File Size';
         Cells[2, 0] := 'Modified Date';}
         col := 0;
         row := 0;
      end;
   UpdateStringGrid;
   myRect.Left := -1;
   myRect.Top := -1;
   myRect.Right := -1;
   myRect.Bottom := -1;
   StringGrid1.Selection := myRect;

   IdFTP1.Host := configuration.EUGene_ftp_site_name;
   IdFTP1.UserName := 'eugene';
   IdFTP1.Password := 'eugene';
   IdFTP1.Port := 21;
   ftpedit.text := IdFTP1.Host;

   connectbutton.enabled := false;
   UploadButton.enabled := false;
   DownloadButton.enabled := false;
   CloseButton.enabled := true;
   HelpButton.enabled := true;
   CancelAbortButton.enabled := false;
   CancelAbortButton.visible := false;

   TransferringData := false;
   SelectedForDownload := false;   {will be true only after they select a grid cell}

   WriteableLocation := extractFilePath (configuration.error_file_name);
   if fileExists (WriteableLocation+'email.txt') then
      begin
         try
            assignfile(emailfile, WriteableLocation+'email.txt');
            reset (emailfile);
            if not eof(emailfile) then readln(emailfile, emailstring);
            closefile(emailfile);
            if OK_email(emailstring) then emailedit.text := emailstring;
         except     {if trouble opening reading, don't worry about it.}
         end;
      end;
end;

procedure TFTP_Form.FormDestroy(Sender: TObject);
   begin
      {ftpfilesname.free;
      modifdate.free;
      size.free;
      attribute.free;}
   end;


   { ---------------------------------------------------------------------------  }
   {  Main form buttons and boxes click/change here.  }

procedure TFTP_Form.EmailEditChange(Sender: TObject);
   begin
      if OK_email (emailedit.Text) then connectbutton.enabled := true
      else connectbutton.enabled := false;
   end;

procedure TFTP_Form.UsePassiveClick(Sender: TObject);
begin
  IdFTP1.Passive := UsePassive.Checked;
end;

procedure TFTP_Form.CloseButtonClick(Sender: TObject);
   var emailfile : textfile;
   begin
      if IdFTP1.Connected then
        begin
           try
              disable_most_buttons;   {keeps close, help}
              IdFTP1.Disconnect;
              FTP_Form.connectButton.enabled := true;
              StatusBar1.Panels[0].Text := 'Disconnected';
           except
              FTP_Form.connectButton.enabled := true;
              FTP_Form.DownloadButton.enabled := false;
              FTP_Form.UploadButton.enabled := false;
              FTP_Form.CloseButton.enabled := true;
              FTP_Form.HelpButton.enabled := true;
              FTP_Form.CancelAbortButton.enabled := false;
              FTP_Form.CancelAbortButton.visible := false;
           end;
        end;

      {Save email address}
      if OK_email(emailedit.text) then
         begin
            try
               assignfile(emailfile, configuration.eugene_directory+'\email.txt');
               rewrite (emailfile);
               writeln (emailfile, emailedit.text);
               closefile(emailfile);
            except     {if trouble opening reading, don't worry about it.}
            end;
         end;

      close;
   end;

procedure TFTP_Form.HelpButtonClick(Sender: TObject);
begin
   ShowMessage ('After connecting to the EUGene website, you may select datasets to download to your PC.  You may also upload datasets to the EUGene website (but note that we recommend using the "User Data|Prepare User Dataset for Submission" menu for this task');
end;

procedure TFTP_Form.CancelAbortButtonClick(Sender: TObject);
begin
   if transferringdata then
      begin
         AbortTransfer := true;
         {don't do anything else - let the work unit procedures handle further steps}
      end
   else
      begin
         IdFTP1.Abort;
      end;
end;



   { --------------------------------------------------------------------   }
   {Main ftp functions here }

procedure TFTP_Form.IdFTP1OnWorkBegin(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCountMax: Integer);
begin
  TransferringData := true;
  AbortTransfer := false;
  CancelAbortButton.Enabled := true;
  CancelAbortButton.Visible := true;
  StatusBar1.Panels[1].Text := 'Transfer beginning';
  STime := Now;
  if AWorkCountMax > 0 then ProgressBar1.Max := AWorkCountMax
     else ProgressBar1.Max := BytesToTransfer;
  ProgressBar1.show;
  ProgressBar1.bringtofront;
  AverageSpeed := 0;
end;

procedure TFTP_Form.IdFTP1OnWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  CancelAbortButton.Enabled := false;
  CancelAbortButton.Visible := false;
  If AbortTransfer then
     StatusBar1.Panels[1].Text := 'Transfer interrupted'
  else StatusBar1.Panels[1].Text := 'Transfer complete';
  StatusBar1.Panels[0].Text := 'Connected';
  BytesToTransfer := 0;
  TransferringData := false;
  ProgressBar1.Position := 0;
  AverageSpeed := 0;
  {AbortTransfer := false;  Leave AbortTransfer=true so external procedures know it was aborted.}
  ProgressBar1.hide;
end;

procedure TFTP_Form.IdFTP1OnWork(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
Var
  S: String;
  TotalTime: TDateTime;
  H, M, Sec, MS: Word;
  DLTime: Double;
begin
  TotalTime :=  Now - STime;
  DecodeTime(TotalTime, H, M, Sec, MS);
  Sec := Sec + M * 60 + H * 3600;
  DLTime := Sec + MS / 1000;
  if DLTime > 0 then
  AverageSpeed := {(AverageSpeed + }(AWorkCount / 1024) / DLTime{) / 2};

  {BytesSent :=
  BytesRecvd := }

  S := FormatFloat('0.00 KB/s', AverageSpeed);
  case AWorkMode of
     wmRead: StatusBar1.Panels[1].Text := 'Received '+IntToStr(AWorkCount)+' bytes of '+IntToStr(BytesToTransfer)+'; Download speed ' + S;
     wmWrite: StatusBar1.Panels[1].Text := 'Sent '+IntToStr(AWorkCount)+' bytes of '+IntToStr(BytesToTransfer)+'; Upload speed ' + S;
  end;
  Application.processmessages;
  
  if AbortTransfer then
     begin
        IdFTP1.Abort;
        FTP_Form.connectButton.enabled := true;
        FTP_Form.CloseButton.enabled := true;
        FTP_Form.HelpButton.enabled := true;
        If selectedForDownload then
           FTP_Form.DownloadButton.enabled := true
        else FTP_Form.DownloadButton.enabled := false;
        If IdFTP1.connected then
           FTP_Form.UploadButton.enabled := true
        else FTP_Form.UploadButton.enabled := false;
        FTP_Form.CancelAbortButton.enabled := false;
        FTP_Form.CancelAbortButton.visible := false;
     end;

  ProgressBar1.Position := AWorkCount;
end;

procedure TFTP_Form.disconnect;
   begin
        try
           ConnectButton.enabled := false;
           if TransferringData then
              begin
                 AbortTransfer := true;    {allow transfer mode to finish packet, then disconnect}
                 TransferringData := false;
              end;
           {Now regular disconnect}
           disable_most_buttons;   {keeps close, help}
           IdFTP1.Quit;
           {window status lines updated in ondisconnect event.}
        finally     {At this point have closed the connection}
           SelectedForDownload := false;   {will be true only after they select a grid cell}
           FTP_Form.connectButton.enabled := true;
           FTP_Form.DownloadButton.enabled := false;
           FTP_Form.UploadButton.enabled := false;
           FTP_Form.CloseButton.enabled := true;
           FTP_Form.HelpButton.enabled := true;
           FTP_Form.CancelAbortButton.enabled := false;
           FTP_Form.CancelAbortButton.visible := false;
           ConnectButton.Caption := 'Connect';
           ConnectButton.Enabled := true;
           ConnectButton.Default := true;

           {ftpfilesname.clear;
           modifdate.clear;
           size.clear;
           attribute.clear;}
           SetLength(ServerUserDataSets, 0);
           UpdateStringGrid;
        end;
   end;

procedure TFTP_Form.ConnectButtonClick(Sender: TObject);
   var LocalFile: string;
       UserFileName : string;
       x, num_files : integer;
       UserFileListFile : textfile;
begin
  if IdFTP1.Connected then
     begin       {disconnect}
        disconnect;
     end           {disconnect section}
  else
     begin         {connect section}
        {Now disconnected}
        try
           disable_most_buttons;   {keeps cancel, close, help}
           if not OK_email (emailedit.text) then ShowMessage ('Please enter your email address to connect to the EUGene server.')
           else
              begin
                 Memo1.Lines.Add('Connecting to EUGene server...');
                 StatusBar1.Panels[0].Text := 'Connecting';
                 {This will establish connection and set .systemdesc property of IdFTP with
                  info about the server.}

                 try
                    IdFTP1.Connect;
                    {If reach here, am connected. }

                    {This ChangeDir command seems to be causing trouble now, which
                     it didn't seem to in the past.  But, drop it.} 
                    IdFTP1.ChangeDir('\');
                    ConnectButton.Caption := 'Disconnect';
                    ConnectButton.Enabled := true;
                    ConnectButton.Default := false;
                    {Now get file list}
                    Memo1.Lines.Add('Downloading user file list from EUGene server.');
                    try
                       try
                         {First get the file with user data file information.}
                         UserFileName := WriteableLocation+EugeneFileListName;
                         IdFTP1.get(EugeneFileListName, userFileName, true);

                         {** Now have the file.  Need to display file names}
                         reset (UserFileListFile, UserFileName);
                         readln (UserFileListFile);   {first line is blank}
                         SetLength(ServerUserDataSets, 0);
                         while not eof (UserFileListFile) do
                            begin
                               SetLength(ServerUserDataSets, Length(ServerUserDataSets) + 1);
                               ServerUserDataSets[Length(ServerUserDataSets)-1].datasetname := read_csv_string (UserFileListFile);
                               num_files := read_csv_int (UserFileListFile);
                               SetLength(ServerUserDataSets[Length(ServerUserDataSets)-1].datasets, num_files);
                               for x := 0 to num_files-1 do
                                  ServerUserDataSets[Length(ServerUserDataSets)-1].datasets[x] := read_csv_string (UserFileListFile);
                               readln(UserFileListFile);
                            end;
                         CloseFile (UserFileListFile);
                         updatestringgrid;
                       finally
                         Memo1.Lines.Add('User file list transfer complete.');
                         StringGrid1.hint := 'Click on a dataset to select it for transfer';
                         FTP_Form.connectButton.enabled := true;
                         FTP_Form.DownloadButton.enabled := false;
                         FTP_Form.UploadButton.enabled := true;
                         FTP_Form.CloseButton.enabled := true;
                         FTP_Form.HelpButton.enabled := true;
                         FTP_Form.CancelAbortButton.enabled := false;
                         FTP_Form.CancelAbortButton.visible := false;
                       end;
                    except
                      MessageDlg ('Unable to obtain user file list from EUGene server.  Check for possible network traffic or slow (timed out) connection.', mterror, [mbOK], 0);
                      FTP_Form.connectButton.enabled := true;
                      FTP_Form.DownloadButton.enabled := false;
                      FTP_Form.UploadButton.enabled := false;
                      FTP_Form.CloseButton.enabled := true;
                      FTP_Form.HelpButton.enabled := true;
                      FTP_Form.CancelAbortButton.enabled := true;
                      FTP_Form.CancelAbortButton.visible := true;
                    end;
                 except
                    MessageDlg ('Unable to connect to EUGene server:  Ensure network or modem connection is working and try again.', mterror, [mbOK], 0);
                    FTP_Form.connectButton.enabled := true;
                    FTP_Form.DownloadButton.enabled := false;
                    FTP_Form.UploadButton.enabled := false;
                    FTP_Form.CloseButton.enabled := true;
                    FTP_Form.HelpButton.enabled := true;
                    FTP_Form.CancelAbortButton.enabled := true;
                    FTP_Form.CancelAbortButton.visible := true;
                 end;
              end;
        except
           FTP_Form.connectButton.enabled := true;
           FTP_Form.DownloadButton.enabled := false;
           FTP_Form.UploadButton.enabled := false;
           FTP_Form.CloseButton.enabled := true;
           FTP_Form.HelpButton.enabled := true;
           FTP_Form.CancelAbortButton.enabled := true;
           FTP_Form.CancelAbortButton.visible := true;
        end;
     end;
end;

{This is the onconnect event.  Not much done here, as functions following success
 are processed in the button click procedure.}
procedure TFTP_Form.IdFTPOnConnected(Sender: TObject);
begin
   ConnectButton.Caption := 'Disconnect';
   ConnectButton.enabled := true;
   Memo1.Lines.Add('Connected to EUGene server');
   StatusBar1.Panels[0].Text := 'Connected';
   StringGrid1.hint := 'Click on a dataset to select it for transfer';
end;

{This is the ondisconnect event.  Not much done here, as functions following success
 are processed in the button click procedure.}
procedure TFTP_Form.IdFTP1OnDisconnected(Sender: TObject);
begin
   ConnectButton.Caption := 'Connect';
   ConnectButton.enabled := true;
   Memo1.Lines.Add('Disconnected from EUGene server');
   StatusBar1.Panels[0].Text := 'Disconnected';
   ProgressBar1.hide;
   StringGrid1.hint := '';
   StringGrid1.Cells[0, 0] := '  Not connected';

   SelectedForDownload := false;   {will be true only after they select a grid cell}
   UploadButton.enabled := false;
   DownloadButton.enabled := false;
end;


procedure TFTP_Form.UploadButtonClick(Sender: TObject);
var
  filenames : Tstrings;
  add_files, can_transfer : boolean;
  extension_data_array : extension_data_type;
  x : integer;
  transfer_bad : boolean;
  
   {When Button is clicked, an Open Dialog is displayed to select a file on the local
   computer to upload to the remote host. }

   procedure transfer_up (localFile : string; var transfer_bad : boolean);
   var RemoteFile: string;
       tempfile : file of byte;
   begin
       RemoteFile := ExtractFileName(LocalFile);
       Memo1.Lines.Add('Uploading file '+LocalFile);
       Memo1.Lines.Add('User dataset transfer started to "/uploads" server directory');
       StatusBar1.Panels[0].Text := 'Uploading';
       assignfile(tempfile, LocalFile);
       reset(tempfile);

       BytesToTransfer := filesize (tempFile);
       closefile(tempfile);
       BytesRecvd := 0;
       BytesSent := 0;
       try
          {Do not append, that's the final boolean on the put command}
          IdFTP1.Put(LocalFile, RemoteFile, false);
          Memo1.Lines.Add('User dataset transfer complete to "\uploads" directory.');
          Memo1.Lines.Add('Once this file is checked for validity, it will be made available for user downloading.');
          StatusBar1.Panels[0].Text := 'Connected';
       except
          FTP_Form.connectButton.enabled := true;
          FTP_Form.DownloadButton.enabled := false;
          FTP_Form.UploadButton.enabled := false;
          FTP_Form.CloseButton.enabled := true;
          FTP_Form.HelpButton.enabled := true;
          FTP_Form.CancelAbortButton.enabled := true;
          FTP_Form.CancelAbortButton.visible := true;
          IdFTP1.ChangeDir('\');             {change on server to this}
          MessageDlg ('Unable to transfer file '+LocalFile+':  Check that file still exists, is not in use, and retry;  or check for appropriate server permissions.', mterror, [mbOK], 0);
          StatusBar1.Panels[0].Text := 'Incomplete Transfer';
          StatusBar1.Panels[1].Text := 'Incomplete Transfer';
          transfer_bad := true;
       end;
   end;         {proc transfer up}


begin       {upload button click}
  if not IdFTP1.Connected then
     begin
        ShowMessage ('You must be connected to the EUGene server to upload files.  Please connect and try again.');
     end
  else    {am connected}
     begin
        transfer_bad := false;
        if FilesForUploadForm.showmodal = mrOK then
           begin
              try
                 try
                    CancelAbortButton.enabled := true;
                    CancelAbortButton.visible := true;
                    IdFTP1.TransferType := ftASCII;
                    disable_most_buttons;   {keeps cancel, close, help}
                    IdFTP1.ChangeDir('uploads');             {change on server to this}
                    for x := 0 to UploadFileNames.count-1 do
                       transfer_up (UploadFileNames.strings[x], transfer_bad);
                    FTP_Form.connectButton.enabled := true;
                    if SelectedForDownload then
                       FTP_Form.DownloadButton.enabled := true
                    else FTP_Form.DownloadButton.enabled := false;
                    FTP_Form.UploadButton.enabled := true;
                    FTP_Form.CloseButton.enabled := true;
                    FTP_Form.HelpButton.enabled := true;
                    FTP_Form.CancelAbortButton.enabled := false;
                    FTP_Form.CancelAbortButton.visible := false;
                    CancelAbortButton.enabled := false;
                    CancelAbortButton.visible := false;
                 except
                    FTP_Form.connectButton.enabled := true;
                    FTP_Form.DownloadButton.enabled := false;
                    FTP_Form.UploadButton.enabled := false;
                    FTP_Form.CloseButton.enabled := true;
                    FTP_Form.HelpButton.enabled := true;
                    FTP_Form.CancelAbortButton.enabled := true;
                    FTP_Form.CancelAbortButton.visible := true;
                    transfer_bad := true;
                    MessageDlg ('Unable to transfer file or change to uploads directory:  Check for possible connection error.', mterror, [mbOK], 0);
                 end;  {outer except loop}
              finally
                 IdFTP1.ChangeDir('\');             {change on server back to regular}
                 if transfer_bad then
                    showMessage ('File Uploads Incomplete!')
                 else ShowMessage ('File Uploads Complete!  Please notify the EUGene authors that your data has been uploaded.');
              end;
           end
        else
           begin
           {do nothing, no transfer}
           end;
     end;   {am connected}
end;   {proc}


procedure TFTP_Form.DownloadButtonClick(Sender: TObject);
var datasetrow, datasetnum, x : integer;
    filenames : Tstrings;
    add_files, can_transfer, dircreateOK : boolean;
    extension_data_array : extension_data_type;
    outmessage : string;
    transfer_bad : boolean;
    anex:EIdException;

   procedure transfer_down (remoteFile, localfile : string; var transfer_bad : boolean);
   var fsize : integer;
   begin
       Memo1.Lines.Add('Downloading file '+remoteFile);
       Memo1.Lines.Add('User dataset transfer started from "User Data Files" server directory');
       StatusBar1.Panels[0].Text := 'Downloading';
       try
          fSize := IdFTP1.Size(remoteFile);
          BytestoTransfer := fSize;
          BytesRecvd := 0;
          BytesSent := 0;
          CancelAbortButton.enabled := true;
          CancelAbortButton.visible := true;
{FIX ***          if (fSize <> -1) and (fSize < GetDiskSpace(extractfiledrive(localfile))) then
}             IdFTP1.Get(remoteFile, localfile, True);
          If AbortTransfer then
             begin
                Memo1.Lines.Add('User dataset transfer aborted!');
                transfer_bad := true;
             end
          else
             Memo1.Lines.Add('User dataset transfer complete to local directory.');
          CancelAbortButton.enabled := false;
          {CancelAbortButton.visible := false;  Do this in outside procedure}
          If IDFTP1.connected then StatusBar1.Panels[0].Text := 'Connected';
       except
          FTP_Form.connectButton.enabled := true;
          FTP_Form.DownloadButton.enabled := false;
          FTP_Form.UploadButton.enabled := false;
          FTP_Form.CloseButton.enabled := true;
          FTP_Form.HelpButton.enabled := true;
          FTP_Form.CancelAbortButton.enabled := true;
          FTP_Form.CancelAbortButton.visible := true;
          MessageDlg ('Unable to transfer file:  Check for possible network traffic or slow (timed out) connection.', mterror, [mbOK], 0);
          transfer_bad := true;
       end;
   end;

   begin
   transfer_bad := false;
   if not IDFTP1.Connected then
     begin
        ShowMessage ('You are not connected to the EUGene server.  You must be connected to the server to download files.  Please connect and try again.');
        transfer_bad := true;
     end
   else    {am connected}
     begin
        try
           try
              disable_most_buttons;   {keeps cancel, close, help}
              IdFTP1.TransferType := ftASCII;
              filenames := tstringlist.create;
              {Set filenames from downloaded user list and file selection.}
              for datasetrow := StringGrid1.selection.top to StringGrid1.selection.bottom do
                 begin
                    {datasetnum := datasetrow - 1;   {every data set is 1 row off}
                    datasetnum := datasetrow;   {every data set is NO LONGER 1 row off}
                    {for each of these user data sets, add the files}
                    for x := 0 to length(ServerUserDataSets[datasetnum].datasets)-1 do
                       filenames.add(ServerUserDataSets[datasetnum].datasets[x]);
                 end;
              {Now transfer files}
              if not (DirectoryExists (configuration.user_data_files_directory)) then
                 if MessageDlg ('User datasets must be placed in the directory '+configuration.user_data_files_directory+'.  This directory does not exist on your system.  Do you want EUGene to attempt to create this directory?', mtconfirmation, [mbYes, mbNo], 0) = mrYes then
                    begin
                       try
                          dircreateOK := createdir (configuration.user_data_files_directory);
                          if dircreateOK = false then
                             begin
                                MessageDlg ('Unable to create directory.  Is this system write protected or on a server?  User datasets cannot currently be downloaded to this system.  ', mtwarning, [mbOK], 0 );
                                transfer_bad := true;
                             end;
                       except
                          MessageDlg ('Error creating directory.  Is this system write protected or on a server?  User datasets cannot currently be downloaded to this system.  ', mterror, [mbOK], 0 );
                          transfer_bad := true;
                       end;
                    end;

              if not(transfer_bad) and (DirectoryExists (configuration.user_data_files_directory)) then
                 for x := 0 to filenames.count-1 do
                    transfer_down (filenames.strings[x], configuration.user_data_files_directory+filenames.strings[x], transfer_bad);
           finally
              if not(transfer_bad or AbortTransfer) then
                 begin
                    outmessage := 'File Download Complete!  Files: ';
                    for x := 0 to filenames.count-1 do
                       outmessage := outmessage + filenames.strings[x] + ';  ';
                    outmessage := outmessage + 'have been transferred to the '+configuration.user_data_files_directory+' directory on your PC.  ';
                    outmessage := outmessage + 'To access variables from these files, exit EUGene and restart the program.';
                    ShowMessage (outmessage);
                 end
                 else
                    begin
                       outmessage := 'File Download of at least one user data file was incomplete!';
                       ShowMessage (outmessage);
                    end;
              FTP_Form.connectButton.enabled := true;
              FTP_Form.DownloadButton.enabled := true;
              FTP_Form.UploadButton.enabled := true;
              FTP_Form.CloseButton.enabled := true;
              FTP_Form.HelpButton.enabled := true;
              FTP_Form.CancelAbortButton.enabled := false;
              FTP_Form.CancelAbortButton.visible := false;
              Memo1.Lines.Add(outmessage);
              filenames.free;
           end;   {finally}
        except
           {This is just in case form gets disabled somewhere in an
            internal procedure.}
{           raise EIdException;}
           eidexception_handle (handled);
        end;

     end;   {connected}
end;         {procedure}

procedure TFTP_Form.eidexception_handle (var Handled: Boolean);
   begin
   end;


procedure TFTP_Form.IdFTP1Status(axSender: TObject;
  const axStatus: TIdStatus; const asStatusText: String);
begin
  StatusBar1.Panels[1].Text := asStatusText;
end;

end.



{      on EIdAlreadyConnected
         begin
            ShowMessage ('Connection attempted but already connected.');
            handled := true;
         end;

      on EIdClosedSocket
         begin
            ShowMessage ('Write attempted but TCP socket is closed.  Disconnecting...');
            handled := true;
            disconnect;
         end;


on EIdConnClosedGracefully
   {Exception raised when a connection has close gracefully.}


on EIdDnsResolverError
 {Exception raised to indicate TIdDNSResolver errors.}


on EIdIcmpException
   {Exception for ICMP components.}

EIdInvalidServiceName
   {Exception raised for an invalid port service.  }

EIdInvalidSocket
   {Exception raised for a connection closed unexpectedly.}

EIdProtocolReplyError
   {Exception type for protocol errors.}

EIdResponseError
   {Exception for errors in an expected response.}

EIdSetSizeExceeded
   {Represents an FD SET error.}

EIdSilentException
   {Exceptions that behave like VCL EAbort.}

EIdSocketError
   {Exception for Socket errors.}

EIdTFTPAccessViolation
   {FTP access violation exception.}

EIdTFTPAllocationExceeded
   {FTP Allocation exceeded exception.}

EIdTFTPException
   {Trivial FTP Exception type.}

EIdTFTPFileAlreadyExists
   {FTP exception raised when creating a file that already exists.}

EIdTFTPFileNotFound

EIdTFTPIllegalOperation
   {Exception for illegal FTP operations.}

EIdTFTPNoSuchUser
   {Represents an FTP User Authentication error.}

EIdTFTPOptionNegotiationFailed
   {Represent s the error for an invalid FTP Option Negotiation response.}

EIdTFTPUnknownTransferID
   {FTP Exception raised for an Unknown Transfer ID.}


{            on EInOutError do
              begin
                 FileErrorBox.maindo ('Error opening file "'+user_selections.user_specified_dyad_list.file_name+ '"',
                                      'File could not be opened for input.',
                                      'File may be in use by another program, or may be missing.');
                 FileErrorBox.showmodal;
                 raise;
              end;
}
   end;



{     nmftp functions not in indy, so not used.  }




{procedure TFTP_Form.NMFTP1Failure(var Handled: Boolean;
  Trans_Type: TCmdType);
begin
  {always make sure user is not stuck with no form access.}
{  FTP_Form.enable_all_buttons;
  case Trans_Type of
    cmdChangeDir: Memo1.Lines.Add('ChangeDir failed');
    cmdMakeDir: Memo1.Lines.Add('MakeDir failed');
    cmdDelete: Memo1.Lines.Add('Delete failed');
    cmdRemoveDir: Memo1.Lines.Add('RemoveDir failed');
    cmdList: Memo1.Lines.Add('List failed');
    cmdRename: Memo1.Lines.Add('Rename failed');
    cmdUpRestore: Memo1.Lines.Add('UploadRestore failed');
    cmdDownRestore: Memo1.Lines.Add('DownloadRestore failed');
    cmdDownload: Memo1.Lines.Add('Download failed');

    cmdUpload: Memo1.Lines.Add('Upload failed');
    cmdAppend: Memo1.Lines.Add('UploadAppend failed');
    cmdReInit: Memo1.Lines.Add('Reinitialize failed');
    cmdAllocate: Memo1.Lines.Add('Allocate failed');
    cmdNList: Memo1.Lines.Add('NList failed');
    cmdDoCommand: Memo1.Lines.Add('DoCommand failed');
    cmdCurrentDir: Memo1.Lines.Add('CurrentDir failed')
  else showmessage ('Unsupported ftp function '+inttostr(ord(Trans_Type))+ ' not recognized in ftp failure function event: notify programmer of ftp error.');
  end;
end;

procedure TFTP_Form.NMFTP1Success(Trans_Type: TCmdType);
begin
  case Trans_Type of
       cmdList:
         begin
            {show list, then clear FTP list for next listing.}
            {updatestringgrid;}
{            ftpfilesname.clear;
            modifdate.clear;
            size.clear;
            attribute.clear;
            {NMFTP1.FTPDirectoryList.clear;}
            {EUGeneFTPDirectoryList.clear;   }
{         end;
       cmdDownload: Memo1.Lines.Add('File transfer successful');
       cmdUpload: Memo1.Lines.Add('File transfer successful');
       cmdChangeDir:
         begin
           Memo1.Lines.Add('Change Directory to ' + nmftp1.currentdir + ' successful');
           NMFTP1.List;
         end;
       cmdMakeDir: Memo1.Lines.Add('MakeDir successful');
       cmdRemoveDir: Memo1.Lines.Add('RemoveDir successful');
       cmdDelete: Memo1.Lines.Add('Delete successful');
       cmdRename: Memo1.Lines.Add('Rename successful');
       cmdReInit: Memo1.Lines.Add('Reinitialize successful');
       cmdCurrentDir: begin {Memo1.Lines.Add('CurrentDir successful');}
{       end;

       cmdUpRestore: Memo1.Lines.Add('UploadRestore successful');
       cmdDownRestore: Memo1.Lines.Add('DownloadRestore successful');
       cmdAppend: Memo1.Lines.Add('UploadAppend successful');
       cmdAllocate: Memo1.Lines.Add('Allocate successful');
       cmdNList: Memo1.Lines.Add('File name list successful');
       cmdDoCommand:
           begin {Memo1.Lines.Add('Other ftp command successful')}
{           end
       else  ShowMessage ('Unrecognized ftp function '+inttostr(ord(Trans_Type))+ ' in ftp success event: notify programmer of error');
    end;   {case}
{  end;      {proc on success}

{procedure TFTP_Form.NMFTP1UnSupportedFunction(Trans_Type: TCmdType);
begin
  case Trans_Type of
    cmdChangeDir: Memo1.Lines.Add('ChangeDir not supported by this server');
    cmdMakeDir: Memo1.Lines.Add('MakeDir not supported by this server');
    cmdDelete: Memo1.Lines.Add('Delete not supported by this server');
    cmdRemoveDir: Memo1.Lines.Add('RemoveDir not supported by this server');
    cmdList: Memo1.Lines.Add('List not supported by this server');
    cmdRename: Memo1.Lines.Add('Rename not supported by this server');
    cmdUpRestore: Memo1.Lines.Add('UploadRestore not supported by this server');

    cmdDownRestore: Memo1.Lines.Add('DownloadRestore not supported by this server');
    cmdDownload: Memo1.Lines.Add('Download not supported by this server');
    cmdUpload: Memo1.Lines.Add('Upload not supported by this server');
    cmdAppend: Memo1.Lines.Add('UploadAppend not supported by this server');
    cmdReInit: Memo1.Lines.Add('Reinitialize not supported by this server');
    cmdAllocate: Memo1.Lines.Add('Allocate not supported by this server');
    cmdNList: Memo1.Lines.Add('NList not supported by this server');

    cmdDoCommand: Memo1.Lines.Add('DoCommand not supported by this server');
    cmdCurrentDir: Memo1.Lines.Add('CurrentDir not supported by this server')
  else showmessage ('Unsupported ftp function '+inttostr(ord(Trans_Type))+ ' not recognized in unsupported ftp function event: notify programmer of ftp error.');
  end;   {case}

{end;


procedure TFTP_Form.NMFTP1ListItem(Listing: String);
   var x, numsubstrings : integer;
       seenspace : boolean;
       substring : array of string;
begin
    {shouldn't need to call this with parselist=true, but it doesn't
     seem to recognize the server type of la.psu.edu, so it doesn't parse the line.}
      {EUGeneFTPDirectoryList.ParseLine(Listing);   }
      {NMFTP1.FTPDirectoryList.ParseLine (Listing); }

{      numsubstrings := 0;
      {set it so initially thinks there was a space, so that the
       first record counter will be incremented to a 1 properlly.}
{      seenspace := true;
      setlength(substring,length(listing)+1);

      {get rid of leading or trailing funky characters}
{      listing := trim(Listing);

      for x := 1 to length(listing) do
         begin
            if listing[x] <> ' ' then
               begin
                  if seenspace then
                     begin
                        inc(numsubstrings);
                        substring[numsubstrings] := '';
                     end;
                  substring[numsubstrings] := substring[numsubstrings]+listing[x];
                  seenspace := false;
               end
            else {listing = space}
{               begin
                  seenspace := true;
               end;
         end;   {for x ...}

      {should end up with substrings in 0..n;  should be 4 substrings}
{      if not numsubstrings = 4 then
         begin      {This is not a valid file name entry, so don't add}
{         end
      else
         begin
            modifdate.add(substring[1]+' '+substring[2]);
            size.add(substring[3]);
            ftpfilesname.add(substring[4]);
            attribute.add(inttostr(0));
         end;

end;
}

