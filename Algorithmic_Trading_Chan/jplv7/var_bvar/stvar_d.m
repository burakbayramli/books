% PURPOSE: An example of using stvar to run a
%          smooth transition vector autoregression.
%           
%---------------------------------------------------
% USAGE: stvar_d
%---------------------------------------------------

clear all;

load peru.data; % a test data set containing
                % data from the Peruvian Economy:
                % Discount Rate (level), Real Exchange Rate (Log Annual Dif.)
                % GDP (Log Annual Dif.), Price Index (Log Annual Dif.) 
               
% monthly data covers 1994,1 to 2003,6

%Arranging Parameters for simulation
param=[2 1 1 0 1 1.5 100 24 1 1];

% estimate the model
results = stvar(peru,param);

vnames =  ['Discount Rate      ',
           'Real Exchange Rate ',    
           'GDP                ',    
           'Price Index        '];

subplot(4,1,1);
    xlabel('Periods');
    ylabel('Disc. Rate');

subplot(4,1,2);
    xlabel('Periods');
    ylabel('Ex. Rate');

subplot(4,1,3);
    xlabel('Periods');
    ylabel('GDP');

subplot(4,1,4);
    xlabel('Periods');
    ylabel('Prices');

       
%prt(results,vnames);
%cutoff = 0.1;
%pgranger(results,vnames,cutoff);
%plt(results,vnames);




