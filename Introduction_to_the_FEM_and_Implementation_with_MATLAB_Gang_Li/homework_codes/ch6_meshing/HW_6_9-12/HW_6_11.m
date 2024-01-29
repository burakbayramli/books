clear all;
global MAX_LENGTH;

load shape_6_11.dat;  % load the PSLG file
save('shape.dat','shape_6_11','-ascii');
MAX_LENGTH=3;   % maxium edge length allowed
dt;             % call the main meshing program