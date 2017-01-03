% PURPOSE: demo of guerrero()
%          Temporal disaggregation with indicators.
%          Guerrero ARIMA-based method
%---------------------------------------------------
% USAGE: guerrero_d
%---------------------------------------------------

clear all; clc; close all;

% ------------------------------------------------------------
% LOW FREQUENCY DATA
% Mexico. Real Gross Domestic Prices (GDP)
% Sample: 1983-1987
% Unit: Millions of Pesos at the 1980 value
% Source: Guerrero, V. (1990) "Temporal disaggregation of time
% series: an ARIMA-based approach", International Statistical
% Review, vol. 58, p. 44-45.

Y=[ 4628937
    4796050
    4919905
    4725277
    4792936 ];

% ------------------------------------------------------------
% HIGH FREQUENCY DATA
% Mexico. Index of Volume of Industrial Production (IVIP)
% Sample: 1983.01-1987.12
% Unit: Index 1980=100
% Source: Guerrero, V. (1990) "Temporal disaggregation of time
% series: an ARIMA-based approach", International Statistical
% Review, vol. 58, p. 44-45.

x=[ 97.992
93.731
99.788
99.304
97.985
94.682
94.196
95.821
91.800
96.030
95.966
95.088
98.273
101.175
106.182
97.956
103.094
102.468
102.639
102.635
100.906
105.465
104.152
102.896
107.367
106.784
112.865
107.493
109.264
106.387
108.623
106.619
104.466
109.970
107.403
105.975
106.016
106.163
104.836
110.173
108.094
101.333
100.735
99.130
95.123
100.345
99.287
99.771
100.125
99.382
108.456
104.377
108.642
106.483
105.441
106.918
107.337
110.721
111.617
112.003 ];
    
% ---------------------------------------------
% Inputs for td library

% Type of aggregation
ta=1;   
% Frequency conversion 
s=12;    

% Model for  w: (0,1,1)(1,0,1)
rexw.ar_reg = [1];
rexw.d  = 1;
rexw.ma_reg = [1 -0.40];

rexw.ar_sea = [1 0 0 0 0 0 0 0 0 0 0 0 -0.85];
rexw.bd = 0;
rexw.ma_sea = [1 0 0 0 0 0 0 0 0 0 0 0 -0.79];

rexw.sigma = 4968.716^2;

% Model for the discrepancy: (1,2,0)(1,0,0)
% See: Martinez and Guerrero, 1995, Test, 4(2), 359-76.

rexd.ar_reg = [1 -0.43]; 
rexd.d  = 2;
rexd.ma_reg = [1];

rexd.ar_sea = [1 0 0 0 0 0 0 0 0 0 0 0 0.62]; 
rexd.bd = 0;
rexd.ma_sea = [1];

rexd.sigma = 76.95^2;

% Calling the function: output is loaded in a structure called res
res=guerrero(Y,x,ta,s,rexw,rexd);

% Calling printing function
% Name of ASCII file for output
file_sal='guerrero.sal';   
output=0; % Do not include series

% Note: if the Econometric Toolbox is not available, change the
% next line by the followig (output related to ARIMA models is
% missed):
% td_print(res,file_sal,output);  

td_print_G(res,file_sal,output);
edit guerrero.sal;

% Calling graph function
%td_plot(res);
