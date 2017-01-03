%nm1p01b: to read the data file and plot the data
% This program does not work.
% You are supposed to permutate the following statements
%  and modify, if needed, to make it work for its purpose
cd('c:\matlab6p5\nma') %change current working directory
weight=hw(I,2);
load hw.dat
clf, subplot(221)
plot(hw)
subplot(222)
axis([5 7 160 200])
plot(height,weight,height,weight,'+')
[height,I]=sort(hw(:,1)); 
