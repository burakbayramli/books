% PURPOSE: demo of litterman()
%          Temporal disaggregation with indicators.
% 			  Litterman method
%---------------------------------------------------
% USAGE: litterman_d
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
  
% High-frequency data: Spain's Registered exports of goods deflated by 
%                      unit value index.
  
x=[   5162
      5054
      4049
      5196
      4972
      5606
      5844
      6196
      6526
      5671
      5631
      6510
      6575
      6797
      5973
      6796
      8404
      8260
      7058
      7403
      7934
      7762
      7087
      8659
      7471
      8082
      6700
      8117
      8271
      8336
      7698
      8372
      9120
      8911
      8035
      8613
      9725
      9529
      7774
      9295
     10357
     10372
      9056
     10812
     11989
     11839
      9686
     11736
     12878
     12211
     10278
     12321
     13267
     12973
     11268
     15008
     16565
     15641
     13684
     17254
     18613
     17774
     14966
     18543
     19287
     19399
     17299
     21065
     20687
     23215
     21382
     24935
     24256
     25558
     21680
     24951
     25284
     26149
     23344
     27754
     28271
     29835
     26148
     30917
     30494
     30486
     26153
     29930
     28627
     29797 ];
  
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
% Calling the function: output is loaded in a structure called res
res=litterman(Y,x,ta,s,type);
% Calling printing function
% Calling printing function
output=0; % Not include series
td_print(res,file_sal,output);
edit td.sal;
% Calling graph function
td_plot(res);
