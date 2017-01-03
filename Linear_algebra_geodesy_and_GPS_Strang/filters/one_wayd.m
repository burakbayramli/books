%ONE_WAYD  Brute way to create one_way data files

%Kai Borre 01-10-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/22  $

c0 = 299792458;
f1 = 154*10.23e6;
f2 = 120*10.23e6;
lambda1 = c0/f1;
lambda2 = c0/f2;

fidb = fopen('bdata.dat');
[da,count] = fread(fidb,Inf,'double');
rows = count/7;
B = reshape(da,rows,7);
clear da
i1 = [];
for i = 1:rows-1
   if B(i+1,1) < B(i,1)
      i1 = [i1 i];
   end; 
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
   if sats(i) > sats(i-1)
      s1 = [s1 i];
   end; 
end
svs1 = sats([s1]);	    % satellites observed at rover
[nosvs1,nr] = size(svs1);

t1 = zeros(nosvs1,1);
sum = zeros(nosvs1,1);
for t = 1:nosvs1
   for i = 1:mr
      if sats(i) == svs1(t)
         sum(t) = sum(t)+BR(indsv(i),7);
         t1(t) = t1(t)+1;
      end; 
   end; 
end;
for i = 1:nosvs1
   fprintf('Satellite %3.0f has average elevation %4.1f\n',...
                                          s1(i), sum(i)/t1(i));
end

% Reference satellite
[maxv,maxi] = max(t1); % t1 counts number of epochs for each satellite
ar1 = svs1((t1-maxv) == 0);
[y1,in1] = max(sum ./t1);
if any(ar1-svs1(in1))
   refsv = svs1(in1); 
else 
   refsv = svs1(in1);
end %I'm not sure if it's correct

% Cutoff angle set to 15 degrees; deletion of low satellites
svs1(sum ./t1 < 15) = [];
[ms,ns] = size(svs1);
fprintf('Cutoff angle: %3.0f\n', 15)
fprintf('Satellites used: %3.0f\n', svs1)

% Arranging data epoch-by-epoch for all satellites
for i = 1:ms
   for tt = 1:mr
      if svs1(i) == sats(tt),
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
   i = i+1;					% clumsy programming!!
   if i == md
      break;
   end;
   vec = svs1-datar(i,2);
   y = all(vec);
   if y > 0
      del_i = [del_i i];
   end;
end
datar(del_i,:) = [];
[mr,nr] = size(datar);
noepochs = mr/ms;

% Preparation of data from master
[sats,indsv] = sort(BM(:,2));
[mm,nm] = size(BM);
for i = 1:ms
   for tt = 1:mm
      if svs1(i) == sats(tt)
         datam = BM(indsv(1:tt),:); 
      end;
   end; 
end;
[md,nd] = size(datam);
i = 0;
del_i = [];
while 1
   i = i+1;					% clumsy programming!!
   if i == md
      break
   end;
   vec = svs1-datam(i,2);
   y = all(vec);
   if y > 0
      del_i = [del_i i];
   end;
end
datam(del_i,:) = [];
clear BM

% Each row in a data-file has 7 columns with the following contents
%	#1      #2   #3	 #4    #5     #6       #7
%  epoch   sv   P1	Phi1   P2    Phi2   elevation

% printout of one-way data 01-10-97

for k = 1:ms
   start = (k-1)*noepochs+1;
   slut = k*noepochs;
   P1 = datar(start:slut,3);
   Phi1 = datar(start:slut,4)*lambda1;
   P2 = datar(start:slut,5);
   Phi2 = datar(start:slut,6)*lambda2;
   elev = datar(start:slut,7);
   b = [P1 Phi1 P2 Phi2 elev];
   
   fid = fopen(['one_w' int2str(svs1(k)) '.dat'],'w');
   fwrite(fid,b,'double');
   fclose(fid);
end

for k = 1:ms
   start = (k-1)*noepochs+1;
   slut = k*noepochs;
   P1 = datam(start:slut,3);
   Phi1 = datam(start:slut,4)*lambda1;
   P2 = datam(start:slut,5);
   Phi2 = datam(start:slut,6)*lambda2;
   elev = datam(start:slut,7);
   b = [P1 Phi1 P2 Phi2 elev];
   
   fid = fopen(['one_y' int2str(svs1(k)) '.dat'],'w');
   fwrite(fid,b,'double');
   fclose(fid);
end
%%%%%%%%%%one_wayd.m  %%%%%%%%%%%%%%%%%%
