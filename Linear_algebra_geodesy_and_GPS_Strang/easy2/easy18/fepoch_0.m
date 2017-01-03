function [time, dt, sats, eof, datee] = fepoch_0(fid)
% FEPOCH_0   Finds the next epoch in an opened RINEX file with
%	          identification fid. From the epoch line is produced
%	          time (in seconds of week), number of sv.s, and a mark
%	          about end of file. Only observations with epoch flag 0
%	          are delt with.

%Kai Borre 09-14-96; revised 03-22-97; revised Sept 4, 2001
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $
%fide = fopen(fid,'rt');

global sat_index;
time = 0;
dt = 0;
sats = [];
NoSv = 0;
eof = 0;

while 1
   lin = fgets(fid); % earlier fgetl
   answer = findstr(lin,'COMMENT');
   
   if ~isempty(answer);
      lin = fgetl(fid);
   end;
  
   
   if (feof(fid) == 1);
      eof = 1;
      break
   end;
   if ((strcmp(lin(29),'0') == 0) & (size(deblank(lin),2) == 29))
      eof = 1; 
      break
   end; % We only want type 0 data
   if ((strcmp(lin(2),'0') == 1)  &  (strcmp(lin(29),'0') == 1))      
      ll = length(lin)-2;
      if ll > 60, ll = 60; end;
      linp = lin(1:ll);        
      %fprintf('%60s\n',linp);
      [year, lin] = strtok(lin);
      year;
      [month, lin] = strtok(lin);
      [day, lin] = strtok(lin);
      month;
      day;
      [hour, lin] = strtok(lin);
      %hour
      [minute, lin] = strtok(lin);
      %minute
      [second, lin] = strtok(lin);
      %second
      [OK_flag, lin] = strtok(lin);
      h = str2num(hour)+str2num(minute)/60+str2num(second)/3600;
      jd = julday(str2num(year)+2000, str2num(month), str2num(day), h);
      [week, sec_of_week] = gps_time(jd);
      jd;
      time = sec_of_week;
      [NoSv, lin] = strtok(lin,'G');
      
      for k = 1:str2num(NoSv)
         [sat, lin] = strtok(lin,'G');
         prn(k) = str2num(sat);
      end
      
      sats = prn(:);
      dT = strtok(lin);
      if isempty(dT) == 0
         dt = str2num(dT);
      end
      break
      
   end
   
end; 
datee=[str2num(year) str2num(month) str2num(day) str2num(hour) str2num(minute) str2num(second)];

%%%%%%%% end fepoch_0.m %%%%%%%%%
