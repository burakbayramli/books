% PURPOSE: An example of using find_nn()
%          finds an index to the nearest neighbors 
%          demo for a large data set                   
%---------------------------------------------------
% USAGE: find_nnd2
%---------------------------------------------------

clear all;

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

load elect.dat;                    % load data on votes
latt = elect(:,5);
long = elect(:,6);
y = elect(:,7);    % # of voters
y = y./elect(:,8); % percentage of elgible voters voting

m = 5;
index = find_nn(latt,long,m);

% pull out nearest neighbor values from y
y1 = y(index(:,1),1); % pulls out nearest neighbor crime rates

% plot crime rates in each neighborhood vs that in the nearest neighbor
plot(y,y1,'.g');
xlabel('rate of voter participation');
ylabel('nearest neighbor participation rates');
fprintf(1,'in pause mode, hit any key to continue \n');
pause;

y2 = y(index(:,1),1)+y(index(:,2),1); % crime rates in nearest 2 neighbors
y2 = y2/2; % an average of these

% plot crime rates in each neighborhood vs the average in the nearest 2 neighbors
plot(y,y2,'.r');
xlabel('voter participation rates');
ylabel('average of 2 nearest neighbor participation rates');
fprintf(1,'in pause mode, hit any key to continue \n');
pause;

% crime rates in nearest 5 neighbors
y5 = y(index(:,1),1)+y(index(:,2),1)+y(index(:,3),1)+y(index(:,4),1)+y(index(:,5),1);
y5 = y5/5; % an average of these

% plot crime rates in each neighborhood vs the average in the nearest 2 neighbors
plot(y,y5,'.r');
xlabel('voter participation rates');
ylabel('average of 5 nearest neighbor participation rates');


