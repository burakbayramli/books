% PURPOSE: An example of two types of sampling for rho in the semip_g function
% Gibbs sampling spatial Probit model with individual effects
% using 1996 presidential election data set
%                
%---------------------------------------------------
% USAGE: semip_gd2
%---------------------------------------------------

clear all;
load semip.mat;

vnames = strvcat('y','highs','college','grad','non-white', ...
'income','urban');

% x-matrix contains 3,110 x 6 matrix with:
%   col1 = high school graduates as a percent of population     
%   col2 = college percent  
%   col3 = graduate school 
%   col4 = non-white
%   col5 = median income     
%   col6 = urban  

% z = 0,1 with 0 = Dole wins, 1 = Clinton wins, 3,110 counties
% W = a 48x48 spatial weight matrix (standardized)
% nregions = 48
% regionobs = a 48 x 1 vector with the # of counties in each state
% states organized alphabetically

[n k] = size(x);


ndraw = 2500;
nomit = 500;
prior.rval = 4;
prior.dflag = 1; % metropolis-hastings for rho
prior.rmin = 0;
prior.rmax = 1;

% use M-H sampling for rho
result = semip_g(z,x,W,nregions,regionobs,ndraw,nomit,prior);
prt(result,vnames);

prior.dflag = 0; % use alternative approach to sample for rho

result2 = semip_g(z,x,W,nregions,regionobs,ndraw,nomit,prior);
prt(result2,vnames);

[h1,f1,y1] = pltdens(result.pdraw);
[h2,f2,y2] = pltdens(result2.pdraw);

plot(y1,f1,'.',y1,f2,'o');
legend('Met-H','Inversion');

