function interpolation
									
%program performs multi-stages interpolation on data in a user-specified data file	

clear all;

r = [2 1 1];	% decimation factor array for different stages
l = 4;	% filter length 
alpha = 0.5;	% cut-off frequency 
in = fopen('decimation.dat','r') ;
[x,count] = fscanf(in,'%g',inf);	% data to be decimated
y = x;
fclose(in);
for i=1:length(r)
	y = interp(y,r(i),l,alpha);
end
subplot(1,2,1);stem(x);subplot(1,2,2);stem(y);