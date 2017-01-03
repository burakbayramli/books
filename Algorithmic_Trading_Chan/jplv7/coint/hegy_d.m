%The followig program use the data from UK National Statistics
%Household final consumption expenditure: National concept CP NSA (1955:1 a 2003:2)
%Not seasonally adjusted
%Updated on 26/ 9/2003
%http://www.statistics.gov.uk/statbase/TSDSelection1.asp
%and performs the HEGY test for sesonal unit roots.
%

clear;
%carga los datos
%Debe determinar el rango a partir del archivo de Excel    
rng = 'B2..B195';
y = wk1read('FinalConsumption',0,0,rng);
y=trimr(y,1,0);
y=trimc(y,1,0);
format short;
y=log(y);

%HEGY
%deterministic Components
%det = 1; %no deterministic components
%det = 2; %constant (default)
%det = 3; %constant & 3 seasonal dummies
%det = 4; %constant & trend%
det = 5; %constant & 3 seasonal dummies & trend

%Lenght of Augmented Component (if necessary)
lag=5;

% USAGE: hegy(x,sig,det,alag,fid);
%hegy(y,0.05,det);
hegy(y,0.05,det,lag);

%RATS Procedure (@HEGYQNEW, Written by Jesper Hansson) for the same series.
%http://www.estima.com/Unit%20root.shtml

%******************************* HEGY-tests **************************************
%Testing for seasonal integration in series: LABPB
%Effective sample: 1957:02 to 2003:02
%Aux.regr.    ´t1´      ´t2´      ´t3´      ´t4´     ´F3&4´   LM-sign    Lags
%-            0.973    -1.627    -0.896    -0.585     0.574     0.107   123456
%I           -1.351    -1.602    -0.939    -0.609     0.624     0.119   1234567
%I,SD        -1.304    -2.121    -3.498    -1.507     7.251     0.207   12345
%I,Tr        -2.226    -1.600    -0.919    -0.626     0.616     0.111   1234567
%I,SD,Tr     -1.767    -2.087    -3.522    -1.416     7.203     0.115   12345
%*********************************************************************************


