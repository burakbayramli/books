% PURPOSE: An example of using make_neighborsw
%          a nearest neighbor spatial weight matrix
%          on a large data set                   
%---------------------------------------------------
% USAGE: make_neighborswd2
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

clear all;

load elect.dat;                    % load data on votes
latt = elect(:,5);
long = elect(:,6);


W2 = make_neighborsw(long,latt,2); 

% 4 neighbors
W4 = make_neighborsw(long,latt,4);

subplot(2,1,1),
spyc(W2,'.r',1);
subplot(2,1,2),
spyc(W4,'.g',1);
