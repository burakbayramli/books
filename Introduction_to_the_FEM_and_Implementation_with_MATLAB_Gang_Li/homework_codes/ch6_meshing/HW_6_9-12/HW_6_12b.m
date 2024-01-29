clear all;
global MAX_LENGTH;

load shape_6_12b.dat; % load the PSLG file
save('shape.dat','shape_6_12b','-ascii');
MAX_LENGTH=.07   % maxium edge length allowed
dt;              % call the main meshing program