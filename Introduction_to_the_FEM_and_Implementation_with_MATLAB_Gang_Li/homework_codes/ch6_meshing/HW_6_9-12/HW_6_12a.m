clear all;
global MAX_LENGTH;

load shape_6_12a.dat; % load the PSLG file
save('shape.dat','shape_6_12a','-ascii');
MAX_LENGTH=.4;   % maxium edge length allowed
dt;             % call the main meshing program