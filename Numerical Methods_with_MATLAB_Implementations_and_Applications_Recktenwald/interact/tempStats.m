function tempStats
% tempStats  Load data from PDXtemp.dat and compute simple statistics
%            on temperature data in the file

fp = fopen('PDXthead.dat','r')
headings = fgetl(fp)
data = fscanf(fp,'%f');
size(data)
data(1:12)
d = reshape(data,4,12);
d(1:4,1:4)
d = d'
% or one step:  d = reshape(data,4,12,)';
T = d(:,2:4);
Thigh_max = max(T(:,1))
Tlow_min  = min(T(:,2))
Tave_ave  = mean(T(:,3))
