%ASH_DD  Arrangement and Formatting of Double Differenced Code
%	      and Phase Observations.

%Kai Borre 04-28-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/23  $

%THIS SAMPLE CODE DOES NOT ACCOUNT FOR CYCLE SLIPS

% Initial computations of constants
v_light = 299792458; 	    % vacuum speed of light m/s
f1 = 154*10.23E6;		       % L1 frequency Hz
f2 = 120*10.23E6;		       % L2 frequency Hz
lambda1 = v_light/f1;	    % wavelength on L1:  .19029367  m
lambda2 = v_light/f2;	    % wavelength on L2:  .244210213 m
alpha1 = f1^2/(f1^2-f2^2);	 % parameters for ionosphere free
alpha2 = -f2^2/(f1^2-f2^2); % combination
omegae = 7.292115147e-5;	 % rotation rate of the earth rad/s
Big = 10^10;

fidb = fopen('bdata.dat');
[da,count] = fread(fidb,Inf,'double');
rows = count/7;
B = reshape(da,rows,7);
clear da
i1 = [];
for i = 1:rows-1
   if B(i+1,1) < B(i,1), i1 = [i1 i]; end; 
end
BR = B(1:i1,:);	    % rover data
BM = B(i1+1:rows,:);  % master data
clear B

% Further preparation of data from rover
[sats,indsv] = sort(BR(:,2));
[mr,nr] = size(BR);
s1 = [];
sats(1) = 0;
for i = 2:mr
   if sats(i) > sats(i-1), s1 = [s1 i]; end; 
end
svs1 = sats([s1]);  % satellites observed at rover
[nosvs1,nr] = size(svs1);

t1 = zeros(nosvs1,1);
sum = zeros(nosvs1,1);
for t = 1:nosvs1
   for i = 1:mr
     if sats(i) == svs1(t), sum(t) = sum(t)+BR(indsv(i),7);
         t1(t) = t1(t)+1; 
     end;
  end;
end;
for i = 1:nosvs1
   fprintf('Satellite %3.0f has average elevation %4.1f\n',...
                                         svs1(i), sum(i)/t1(i)); 
end

% Reference satellite
[maxv,maxi] = max(t1); % t1 counts number of epochs for each satellite
ar1 = svs1((t1-maxv) == 0);
[y1,in1] = max(sum ./t1);
if any(ar1-svs1(in1))
    refsv = svs1(in1);  
else
    refsv = svs1(in1);
end 

% Cutoff angle set to 15 degrees; deletion of low satellites
svs1(sum ./t1 < 15) = [];
[ms,ns] = size(svs1);
fprintf('Cutoff angle: %3.0f\n', 15)
fprintf('Satellites used: %3.0f\n', svs1)

% Arranging data epoch-by-epoch for all satellites
for i = 1:ms
   for tt = 1:mr
      if svs1(i) == sats(tt)
         datar = BR(indsv(1:tt),:); 
      end;
   end;
end;
[md,nd] = size(datar);
clear BR

% Deletion of data for low satellites
i = 0;
del_i = [];
while 1
   i = i+1;
   if i == md, break, end;
   vec = svs1-datar(i,2);
   y = all(vec);
   if y > 0, del_i = [del_i i]; end;
end
datar(del_i,:) = [];
[mr,nr] = size(datar);
noepochs = mr/ms;

% Preparation of data from master
[sats,indsv] = sort(BM(:,2));
[mm,nm] = size(BM);
for i = 1:ms
   for tt = 1:mm
      if svs1(i) == sats(tt),
         datam = BM(indsv(1:tt),:); 
      end;
  end;
end;
[md,nd] = size(datam);
i = 0;
del_i = [];
while 1
   i = i+1;
   if i == md, break, end;
   vec = svs1-datam(i,2);
   y = all(vec);
   if y > 0, del_i = [del_i i]; end;
end
datam(del_i,:) = [];
clear BM
% Small manipulations around the reference satellite
for i = 1:ms
   if svs1(i) == refsv, break, end;
end;
i_refsv = i;
svs1(i_refsv) = [];

%  Data for the reference satellite copied to own variables; and
%  deleted in original variables datar and datam. This makes life
%  much easier!
refstart = (i_refsv-1)*noepochs+1;
refslut = i_refsv*noepochs;
datarref = datar(refstart:refslut,:);
datamref = datam(refstart:refslut,:);
datar(refstart:refslut,:) = [];
datam(refstart:refslut,:) = [];
% Each row in a data-file has 7 columns with the following contents
%	#1      #2   #3	 #4    #5   #6     #7
%  epoch   sv   P1	 Phi1  P2   Phi2   elevation

wl = [];
n1 = [];
ef = 5;		       % first epoch
el = 90;		       % last epoch
fprintf('\nReference Satellite: %3.0f\n\n', refsv)
fprintf('Begin time %10.0f\n', datar(ef,1))
fprintf('End time   %10.0f\n\n', datar(el,1))
fprintf('Epoch interval %4.0f\n\n', datar(2,1)-datar(1,1))
%%%%%%% end ash_dd.m %%%%%%%%%%%%%%%%%%%%%
