function datafile = bdata(masterfile)
% B_CLOCK Reading of binary P-code data as resulting
%     	 from Z-12 receiver.
%	       Input of b-file from master.
%	       Typical call: bdata('b0810a94.076')

%Kai Borre 08-11-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

global outm

delete clock.dat
c = 299792458;  % vacuum speed of light, m/s
% outm = [];

fid = fopen(masterfile);
A = fread(fid,inf,'char');
[m,n] = size(A);
clear A
frewind(fid)
% Reading the binary observation file
% It starts with a raw-header 90 bytes
versionr = fread(fid,10,'char');  version = setstr(versionr');
raw_version = fread(fid,1,'uchar');
rcvr_typer = fread(fid,10,'char'); rcvr_type = setstr(rcvr_typer');
chan_verr = fread(fid,10,'char'); chan_ver = setstr(chan_verr');
nav_verr = fread(fid,10,'char'); nav_ver = setstr(nav_verr');
capabilityr = fread(fid,1,'int'); capability = int2str(capabilityr'); % ??
reservedr = fread(fid,1,'long'); reserved = int2str(reservedr'); % ??
num_obss = fread(fid,1,'char'); num_obs_types = int2str(num_obss); % ??
sparer = fread(fid,40,'char'); %spare = int2str(sparer')
m = m-90; % Number of bytes left in file
t1 = 0;

while 1
   % Reading of epochs
   % Next follows the raw-nav structure: 67 bytes
   sitenamer = fread(fid,4,'char'); sitename=setstr(sitenamer');
   rcv_time = fread(fid,1,'double');
   if t1 == 0
      start_time = rcv_time; 
   end
   end_time = rcv_time;
   navx = fread(fid,1,'double');
   navy = fread(fid,1,'double');
   navz = fread(fid,1,'double');
   navxdot = fread(fid,1,'float');
   navydot = fread(fid,1,'float');
   navzdot = fread(fid,1,'float');
   navt = fread(fid,1,'double');
   navtdot = fread(fid,1,'double');
   pdop = fread(fid,1,'ushort')/100;
   num_sats = fread(fid,1,'uchar');
   m = m-67-97*num_sats;
   if m <= 0
      break
   end
   outm = [outm; navt navtdot];
   
   % The proper observations:
   % The counter j runs through the individual satellites in each epoch
   % The counter i runs through the various code observations
   % the number of which is num_obs_types. (A bug here at present)
   for j = 1:num_sats
      svprn(j) = fread(fid,1,'uchar');
      elevation(j) = fread(fid,1,'uchar');
      azimuth(j) = fread(fid,1,'uchar')*2;
      chnind(j) = fread(fid,1,'uchar');
      for i = 1:3
         % i = 1: C/A-code on L1
         % i = 2:   P-code on L1
         % i = 3:   P-code on L2
         rawrange(j,i) = fread(fid,1,'double')*c;
         smth_corr(j,i) = fread(fid,1,'float');
         smth_count(j,i) = fread(fid,1,'ushort');
         polarity_known(j,i) = fread(fid,1,'char');
         warning(j,i) = fread(fid,1,'uchar');
         goodbad(j,i) = fread(fid,1,'uchar');
         ireg(j,i) = fread(fid,1,'uchar');  % also known as S/N ratio
         qa_phase(j,i) = fread(fid,1,'char');
         doppler(j,i) = fread(fid,1,'long');
         carphase(j,i) = fread(fid,1,'double');
         % Stored observations:
         % receiver_time  svprn  codeL1P  phaseL1P ...
         %		       codeL2P	phaseL2P  elevation
         %	   if i == 3, outm = [outm; navt(j) navtdot(j)];
         %				  rawrange(j,2)... %-smth_corr(j,2) ...
         %				  carphase(j,2)  ....
         %				  rawrange(j,3)... %-smth_corr(j,3)...
         %				  carphase(j,3)  elevation(j)]; end;
         %     if (k==2) & (i==3) & (rcv_time >= start_time) & (rcv_time < end_time)
         %		      outm = [outm; rcv_time svprn(j)...
         %				   rawrange(j,2)... %-smth_corr(j,2) ...
         %				   carphase(j,2) ...
         %				   rawrange(j,3)... %-smth_corr(j,3)...
         %				   carphase(j,3)  elevation(j)]; end;
      end; %i
      t1 = t1+1;
   end; % j
end; % while
%outr([1:18],:)=[]; % first and second epoch deleted due to cold start
start_time %=outr(1,1)
end_time
fidu = fopen('clock.dat','w');
count = fwrite(fidu,outm,'double');
fclose(fidu);

%%%%%%%%% end b_clock.m  %%%%%%%%%%%%%%%%%%%%%
