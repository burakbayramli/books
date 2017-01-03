% PURPOSE: An example of using xy2cont()
%          to produce a sparse spatial weight matrix 
%          using a large data set                   
%---------------------------------------------------
% USAGE: xy2cont_d2
%---------------------------------------------------


% A data set for 1980 Presidential election results covering 3,107 
% US counties. From Pace, R. Kelley and Ronald Barry. 1997. ``Quick
% Computation of Spatial Autoregressive Estimators'',
% in  Geographical Analysis.
% 
%  Variables are:
%  columns 1-4 are census identifiers 
%  column 5  = lattitude
%  column 6  = longitude
%  column 7  = population casting votes
%  column 8  = population over age 19 eligible to vote
%  column 9  = population with college degrees
%  column 10 = homeownership
%  column 11 = income

load elect.dat;                    % load data on votes
latt = elect(:,5);
long = elect(:,6);

% create contiguity matrix (1st order)
[junk W junk] = xy2cont(latt,long);

spy(W);
title('row-stochastic weight matrix 3,107 US counties');
