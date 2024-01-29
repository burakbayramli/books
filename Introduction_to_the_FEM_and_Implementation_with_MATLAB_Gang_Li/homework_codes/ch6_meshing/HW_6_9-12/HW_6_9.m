clear all;
global MAX_LENGTH;

load shape_6_9.dat;   % load the PSLG file
save('shape.dat','shape_6_9','-ascii');
MAX_LENGTH=.05;   % maxium edge length allowed
dt;   % call the main meshing program