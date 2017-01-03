% PURPOSE: demo of bfl()
%         Temporal disaggregation using the Boot-Feibes-Lisman method
%---------------------------------------------------
% USAGE: bfl_d
%---------------------------------------------------

close all; clear all; clc;

% Low-frequency data: Spain's Exports of Goods. 1995 prices

Y=[  20499
     23477
     25058
     27708
     31584
     31898
     30233
     32235
     34049
     36035
     39795
     44299
     47426
     52339
     62949
     69885
     77174
     90133
     96496
    102776
    113026
    115573 ];
  
% ---------------------------------------------
% Inputs for td library

% Type of aggregation
ta=1;   
% Minimizing the volatility of d-differenced series
d=1;
% Frequency conversion 
s=12;    
% Name of ASCII file for output
file_sal='td.sal';   
% Calling the function: output is loaded in a structure called res
res=bfl(Y,ta,d,s);
% Calling printing function
tduni_print(res,file_sal);
edit td.sal;
% Calling graph function
tduni_plot(res);
