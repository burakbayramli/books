function [U2]=fwind_profile(z2);
%Estimating wind at altutude h above 

Z0=0.25;        %Roughness length 
wave=1;         %Wave height
d=0.75*wave;    %Zero plane displacement

z1=3;           %Wind measurement height
U1=10;          %measued wind;


U2=U1*  log((z2-d)/Z0)    /    log((z1-d)/Z0);

