function [sol, histout]=hc_driver
% HC_DRIVER
%
% Use imfil.m to solve the Hydraulic Capture problem.
%
% Arrange the variables nicely.
%
v=zeros(12,1);
v(1) = 150; v(2) = 750; v(3) = 400; v(4) = 750;
v(5) = 250; v(6) = 650; v(7) = 250; v(8) = 450;
v(9) = 0.0064; v(10) = 0.0064; v(11) = -0.0064; v(12) = -0.0064;
%
%
%
working_directory='hc_tmp';
hc_clean(working_directory);

budget=100;
scaledepth=12;
fscale=9.6d4;
options=imfil_optset('scaledepth',scaledepth,'fscale',fscale,'quasi','bfgs');
options=imfil_optset('stencil_wins',1,options);
bounds=zeros(12,2);
%
% Bounds on well locations. Keep them away from the boundary
%
for i=1:8
    bounds(i,1)=10.0;
    bounds(i,2)=990.0;
end
%
% Constraints on pumping rates.
%
for i=9:12
    bounds(i,1)=-.0064;
    bounds(i,2)=.0064;
end
[sol, histout]=imfil(v, @hceval, budget, bounds,options,working_directory);


function hc_clean(working_directory)
%
% Start things off right by clearing the old files from the
% working directory.
%
mkdir=['mkdir ',working_directory];
headfiles=['rm -f ',working_directory,'/HC.hed*'];
wellfiles=['rm -f ',working_directory,'/HC.wel*'];
configfiles=['rm -f ',working_directory,'/Config*'];
[ss,rr]=system(mkdir);
[ss,rr]=system(headfiles);
[ss,rr]=system(wellfiles);
[ss,rr]=system(configfiles);
