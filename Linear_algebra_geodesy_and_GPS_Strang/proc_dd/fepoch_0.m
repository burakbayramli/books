function [time, dt, sats, eof] = fepoch_0(fid)
% FEPOCH_0   Finds the next epoch in an opened RINEX file with
%	          identification fid. From the epoch line is produced
%	          time (in seconds of week), number of sv.s, and a mark
%	          about end of file. Only observations with epoch flag 0
%	          are delt with.

%Kai Borre 09-14-96; revised 03-22-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

global sat_index

time = 0;
dt = 0;
sats = [];
eof = 0;

while 1
   lin = fgets(fid); % earlier fgetl
   answer = findstr(lin,'COMMENT');
   if ~isempty(answer)
      lin = fgetl(fid);
   end;
   if (feof(fid) == 1)
      eof = 1;
      break
   end;
   if ((strcmp(lin(29),'0') == 0) & size(deblank(lin),2) == 29)
      eof = 1; 
      break
   end; % We only want type 0 data
   if ((strcmp(lin(2),'9') == 1)  &  (strcmp(lin(29),'0') == 1))      
      ll = length(lin)-2;
      if ll > 60, ll = 60; end
      linp = lin(1:ll);        
      fprintf('%60s\n',linp)
      [year, lin] = strtok(lin);
      [month, lin] = strtok(lin);
      [day, lin] = strtok(lin);
      [hour, lin] = strtok(lin);
      [minute, lin] = strtok(lin);
      [second, lin] = strtok(lin);
      [OK_flag, lin] = strtok(lin);
      h = str2num(hour)+str2num(minute)/60+str2num(second)/3600;
      jd = julday(str2num(year)+1900, str2num(month), str2num(day), h);
      [week, sec_of_week] = gps_time(jd);
      time = sec_of_week;
      [NoSv, lin] = strtok(lin);
      for k = 1:str2num(NoSv)
         [sat, lin] = strtok(lin);
         prn(k) = str2num(sat);
      end
      sats = prn(:);
      dT = strtok(lin);
      if isempty(dT) == 0
         dt = str2num(dT);
      end
      break
   end
end; % while
%%%%%%%%% end fepoch_0.m %%%%%%%%%
