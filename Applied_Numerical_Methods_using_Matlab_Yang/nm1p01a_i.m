%nm1p01a: input data pairs and save them into an ASCII data file
% This program does not work.
% You are supposed to permutate the following statements
%  and modify, if needed, to make it work for its purpose
clear
k=0;
while 1
end
k=k+1;
x(k,1)=h;
h=input('Enter height:')
x(k,2)=input('Enter weight:')
if isempty(h), break; end
cd('c:\matlab6p5\nma') %change current working directory
filename=input('Enter filename(.dat):','s');
filename=[filename '.dat']; %string concatenation
save(filename,'x','/ascii')
