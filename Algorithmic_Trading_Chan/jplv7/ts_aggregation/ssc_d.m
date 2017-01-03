% PURPOSE: demo of ssc()
%          Temporal disaggregation with indicators.
%          Santos Silva-Cardoso method
%---------------------------------------------------
% USAGE: ssc_d
%---------------------------------------------------

close all; clear all; clc;

% Low-frequency data: annual US personal consumption, 1953-1984. 
% Data from Greene (1997) "Econometric analysis", table 17.5

Y = [  1453.60
       1480.10
       1576.40
       1621.50
       1655.30
       1671.80
       1751.70
       1808.10
       1845.60
       1928.00
       2001.90
       2111.80
       2229.80
       2342.60
       2410.60
       2537.60
       2631.60
       2688.30
       2787.10
       2948.10
       3071.50
       3051.20
       3117.60
       3292.40
       3457.40
       3612.70
       3710.20
       3727.30
       3801.80
       3853.10
       4036.70 
       4249.60 ];
    
% High-frequency data: quarterly US personal personal disposable income, 
% 1953.I - 1984.IV. Seasonally adjusted data.
% Data from Greene (1997) "Econometric analysis", table 17.5

x=[   395.5
      401.0
      399.7
      400.2
      399.7
      397.3
      403.8
      411.8
      414.7
      423.8
      430.8
      437.6
      441.2
      444.7
      446.6
      452.7
      452.6
      455.4
      457.9
      456.0
      452.1
      455.1
      464.6
      471.3
      474.5
      482.2
      479.0
      483.1
      487.8
      490.7
      491.0
      488.8
      493.4
      500.7
      505.5
      514.8
      519.5
      523.9
      526.7
      529.0
      533.3
      538.9
      544.4
      552.5
      563.6
      579.4
      586.4
      593.0
      599.7
      607.8
      623.6
      634.6
      639.7
      642.0
      649.2
      700.7
      665.0
      671.3
      676.5
      682.0
      690.4
      701.9
      703.6
      708.7
      710.4
      717.0
      730.1
      733.2
      737.1
      752.6
      759.7
      756.1
      771.3
      779.7
      781.0
      785.5
      791.7
      798.5
      842.2
      838.1
      855.0
      862.1
      868.0
      873.4
      859.9
      859.7
      859.7
      851.1
      845.1
      891.3
      878.4
      884.9
      899.3
      904.1
      908.8
      914.9
      919.6
      934.1
      951.9
      965.9
      973.5
      982.6
      994.2
     1005.0
     1011.1
     1011.8
     1019.7
     1020.2
     1025.9
     1011.8
     1019.3
     1030.2
     1044.0
     1041.0
     1058.4
     1056.0
     1052.8
     1054.7
     1057.7
     1067.5
     1073.3
     1082.2
     1102.1
     1124.4 
     1147.8
     1165.3
     1176.7 
    1186.9 ];
          
% ---------------------------------------------
% Inputs for td library

% Type of aggregation
ta=1;
% Frequency conversion
s=4;
% Method of estimation
type=1;
% Name of ASCII file for output
file_sal='td.sal';

% Note: the grid search applied in the ssc procedure generates 
% a warning when phi=0. It is easy to modify the code to skip 
% this value but it may create some inconveniences in their connection 
% with the output functions, so I advise to instruct Matlab to avoid 
% sending this message before calling ssc and reset the warning after ssc. 
% Although my experience with ssc is limited, this conditioning problem 
% is not critical for the overall performance of the function.

warning off MATLAB:nearlySingularMatrix

% Calling the function: output is loaded in a structure called res
res=ssc(Y,x,ta,s,type);

%phi=0.259;
%res=ssc_fix(Y,x,ta,s,type,phi);

warning on MATLAB:nearlySingularMatrix

% Calling printing function
output=0; % Include series
td_print(res,file_sal,output);
edit td.sal;
% Calling graph function
td_plot(res);


 
