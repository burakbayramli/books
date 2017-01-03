% PURPOSE: A demo of using the seed option in semip_gc
% Gibbs sampling spatial Probit model with individual effects
% using 1996 presidential election data set
%                
%---------------------------------------------------
% USAGE: test_seed
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


ndraw = 1500;
nomit = 500;
prior.rval = 4;  % heteroscedastic prior
prior.lflag = 0; % no lndet approximation
prior.rmin = 0;  
prior.rmax = 1;


% c-language mex file version takes 25 seconds
prior.seed = 201020;
result = semip_gc(z,x,W,nregions,regionobs,ndraw,nomit,prior);
disp('seed = 201020');
prt(result,vnames);

prior.seed = 1010;
result2 = semip_gc(z,x,W,nregions,regionobs,ndraw,nomit,prior);
disp('seed = 1010');
prt(result2,vnames);

prior.seed = 201020;
result3 = semip_gc(z,x,W,nregions,regionobs,ndraw,nomit,prior);
disp('seed = 201020');
prt(result3,vnames);
