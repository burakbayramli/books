%function  proc_dd(ofile1, ofile2, navfile)
%PROC_DD Processing of double differenced GPS data
%	      as read from RINEX files

%Typical call: proc_dd('site1.96o','site2.96o','site1.nav')

%Kai Borre 03-29-97
%Copyright (c) by Kai Borre
%$Revision: 1.1 $  $Date: 1997/10/29  $

% Some data samples demonstrating different computational behavior

%ofile1 = 'lt031411.97o';
%ofile2 = 'lt041411.97o';
%navfile = 'arunas.nav';


%ofile1 = '08100761.94o'; %These files have cycle slips
%ofile2 = '00050761.94o';
%ofile1 = '08120761.94o';
%navfile = 'fjellera.nav';

%ofile1 = 'pta.96o';      %These files do have cycle slips 
%ofile2 = 'ptb.96o';
%navfile = 'pta.nav';

ofile1 = 'site1.96o';     %These files are free of cycle slips
ofile2 = 'site2.96o';
navfile = 'site1.nav';

%    THIS SAMPLE CODE DOES NOT ACCOUNT FOR CYCLE SLIPS
%    Units are either seconds, meters, or radians

global Obs_types1  Obs_types2
global Eph ambi ty
global ATA ATb Solution
global sat_index
global X_ista1 X_ista2 

v_light = 299792458;   % m/s
f1 = 154*10.23e6;
f2 = 120*10.23e6;

%if Lambda == lambda1
%   Lambda = v_light/f1;
%else
%   Lambda = v_light/f2;
%end
%The choice of L1, or L2 also depends on this selction

 Lambda = v_light/f1;
%Lambda = v_light/f2; %remember to change observation type in sum_norm

%The user first must create an ephemeris file by the following call:
%		rinexe('pta.96n','pta.nav')
%The ephemerides data are now in the file 'pta.nav' in binary form

tic
fid1 = fopen(ofile1,'rt');
fid2 = fopen(ofile2,'rt');
time1 = -1*1.e50;
time2 = -1*1.e50;
[Obs_types1, ant_delta1, ifound_types1, eof11] = anheader(ofile1);
[Obs_types2, ant_delta2, ifound_types2, eof22] = anheader(ofile2);
if ((ifound_types1 == 0) | (eof11 == 1))
   error('Basic information is missing in RINEX file')
end;
if ((ifound_types2 == 0) | (eof22 == 1))
   error('Basic information is missing in RINEX file')
end;
NoObs_types1 = size(Obs_types1,2)/2;
NoObs_types2 = size(Obs_types2,2)/2;
ty = 20;       % Maximum number of normal equations 
ATA = zeros(ty,ty);
ATA = ATA + 1.e-20*eye(ty);
ATA(4,4) = 1.e16; 
ATb = zeros(ty,1);
sat_index = zeros(25,1);	% is used in locate.m

fid1 = fopen(ofile1);
fid2 = fopen(ofile2);
% Downloading of ephemeris data
Eph = get_eph(navfile);
first = 0;
if_skipping = 0;
ambi = zeros(10,1);

%For ofile = site1.96o  use while1
%For ofile = pta.96o    using tt = 1:40
%                         yields float ambiguities, but correct vector
%                       using while 1,
%                         yields float ambiguities, but correct vector
% For ofile = 08100761.94o use tt = 1:20 to get reasonable results
%   PRN31, PRN26 and PRN16 have cycle slips. So omit these PRNs by 
%     setting it in sum_norm  and try tt = 1:80

%for tt = 1:40
while 1   
   ifile_to_read = ofile1;
   if (time2 < time1), ifile_to_read = ofile2; end;
   
   if (ifile_to_read == ofile1)
      [time1, dt1, sats1, eof1] = fepoch_0(fid1);   %
      if (eof1 == 1), break; end;
   else
      [time2, dt2, sats2, eof2] = fepoch_0(fid2);   %
      if (eof2 == 1), break; end;
   end;
   if if_skipping  == 0
      [time2, dt2, sats2, eof2] = fepoch_0(fid2);	 %
   end
   while (round(time1) < round(time2))
      [time1, dt1, sats1, eof1] = fepoch_0(fid1);   %
      if_skipping = 1;
   end
   NoSv1 = size(sats1,1);
   NoSv2 = size(sats2,1);
      
   %Start of synchronous reading
   if (round(time1) == round(time2))
      
      obs1 = grabdata(fid1, NoSv1, NoObs_types1);
      obs2 = grabdata(fid2, NoSv2, NoObs_types2);         
        
      if first == 0
         i = fobs_typ(Obs_types1,'P2');
         pos = b_point(obs1(:,i),sats1,time1,navfile);
         X_ista1 = pos(1:3,1);
         j = fobs_typ(Obs_types2,'P2');
         pos = b_point(obs2(:,j),sats2,time2,navfile);
         X_ista2 = pos(1:3,1);
         Solution(1:3,1) = X_ista2 - X_ista1;
         first = first+1;
      end
      if first > 0
         sum_norm(time1, sats1, obs1, ant_delta1, ...
                          time2, sats2, obs2, ant_delta2, Lambda);
      end
   end
end
% We delete the zero part of the normals and find the solution
cut = max(find(diag(ATA > 0.0000001)));
ATA(cut+1:20,:) = [];
ATA(:,cut+1:20) = [];
ATb(cut+1:20) = [];
M = inv(ATA);
dSol = M*ATb;
trace = M(1,1)+M(2,2)+M(3,3);
pdop = sqrt(trace)

%LAMBDA decorrelation: We compute the integer ambiguities N,
%       and correct the previous solution
fidlog = fopen('lambda.log','wt');
[N,disall] = lambda(fidlog,cut-4,M(5:cut,5:cut),dSol(5:cut,1));
dSol = inv(ATA(1:3,1:3))*(ATb(1:3,1)-ATA(1:3,5:cut)*N);
baseline = X_ista2-X_ista1-dSol(1:3,1);
fprintf('\n Vector from 1 to 2:%12.3f  %12.3f  %12.3f\n',...
                            baseline(1), baseline(2), baseline(3)) 

fclose('all');
toc
%%%%%%%%% end proc_dd.m %%%%%%%%%
