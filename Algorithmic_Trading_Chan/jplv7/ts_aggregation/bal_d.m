% PURPOSE: Demo of bal()
%          Proportional transversal balancing
%---------------------------------------------------
% USAGE: bal_d
%---------------------------------------------------

clc; clear all; close all;

% Unbalanced time series (read columnwise)

y=[120 20 55
   130 22 66 
   190 33 99
   170 35 110 ];

% Transversal constraints

z=[ 210
    208
    300
    310 ];

% Calling balancing function    

yb = bal(y,z);

% Check

initial_discrepancy = sum(y')' - z
final_discrepancy   = sum(yb')' - z

% Revision (as %)

p = 100 * ((yb - y) ./ y);

% Final results:

disp ('Initial estimate, final estimate, revision in % (stacked results)');
results=[vec(y) vec(yb) vec(p)]
