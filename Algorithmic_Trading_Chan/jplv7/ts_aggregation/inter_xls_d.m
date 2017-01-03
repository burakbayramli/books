% PURPOSE: demo of inter_xls()
%          Temporal disaggregation with indicators.
%          Interface with Excel
% 			  
%---------------------------------------------------
% USAGE: inter_xls_d
%---------------------------------------------------
close all; clear all; clc;
% Low-frequency data: Spain's Exports of Goods. 1995 prices
Y=[  20499     23477     25058     27708     31584     31898     30233     32235     34049     36035     39795     44299     47426     52339     62949     69885     77174     90133     96496    102776    113026    115573 ];
% High-frequency data: Spain's Registered exports of goods deflated by %                      unit value index.
x=[   5162      5054      4049      5196      4972      5606      5844      6196      6526      5671      5631      6510      6575      6797      5973      6796      8404      8260      7058      7403      7934      7762      7087      8659      7471      8082      6700      8117      8271      8336      7698      8372      9120      8911      8035      8613      9725      9529      7774      9295     10357     10372      9056     10812     11989     11839      9686     11736     12878     12211     10278     12321     13267     12973     11268     15008     16565     15641     13684     17254     18613     17774     14966     18543     19287     19399     17299     21065     20687     23215     21382     24935     24256     25558     21680     24951     25284     26149     23344     27754     28271     29835     26148     30917     30494     30486     26153     29930 ]; 
% ---------------------------------------------% Inputs for td library
% Type of aggregationta=1;   
% Frequency conversion s=4;    
% Method of estimationtype=1;

% Fixed innovation parameter
ip=0.7;

% Degrre of differencing
d=1;

% Method: flax1 =
%  1 = Boot-Feibes-Lisman
%  2 = Denton
%  3 = Fernandez
%  4 = Chow-Lin
%  5 = Litterman
%  6 = Santos-Cardoso
%  7 = Chow-Lin fixed innnov. param.
%  8 = Litterman fixed innnov. param.
%  9 = Santos-Cardoso fixed innnov. param.

flax1=2;

% Output: flax2 = 
% 1 = only temporally disaggregated time series y: nx1
% 2 = 1 + s.e. lower bound upper bounf limits residuals y: nx5
% 3 = 2 + output in ASCII file (called file_name)

flax2=3;
% Name of ASCII file for outputfile_name='td.sal';   
% Calling the function: output is loaded in a matrix yy = inter_xls(Y,x,ta,s,type,ip,d,flax1,flax2,file_name);

%res=bfl(Y,ta,d,s);
res=denton_uni(Y,x,ta,d,s);
%res=fernandez(Y,x,ta,s);
%res=chowlin(Y,x,ta,s,type);
%res=litterman(Y,x,ta,s,type);
%res=ssc(Y,x,ta,s,type);
%res=chowlin_fix(Y,x,ta,s,type,ip);
%res=litterman_fix(Y,x,ta,s,type,ip);
%res=ssc_fix(Y,x,ta,s,type,ip);

z=res.y;

delta=y(:,1)-z;

[min(delta) mean(delta) max(delta)]
