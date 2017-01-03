% PURPOSE: demo of pairs (pairwise scatterplots)
%          
% 
%---------------------------------------------------
% USAGE: pairs_d
%---------------------------------------------------

% generate correlated data
n = 100;
y1 = randn(n,1);
y2 =  2*y1 + randn(n,1);
y3 = -2*y2 + randn(n,1);
y4 = randn(n,1); % uncorrelated with y1,y2,y3

y = [y1 y2 y3 y4];

vnames = ['apples ',
          'oranges',
          'pairs  ',
          'ha ha  '];


pairs(y,vnames);