clear all;
global MAX_LENGTH;

load shape_6_10.dat; % load the PSLG file
save('shape.dat','shape_6_10','-ascii');
MAX_LENGTH=1;   % maxium edge length allowed
dt;       % call the main meshing program