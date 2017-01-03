% PURPOSE: An example of using find_nn()
%          finds an index to the nearest neighbors 
%          demo for a small data set                   
%---------------------------------------------------
% USAGE: find_nnd
%---------------------------------------------------

clear all;

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
y = anselin(:,1); % neighborhood crime rates
xc = anselin(:,4);
yc = anselin(:,5);
% To find indexes to m neighbors
% (where m is the # of nearest neighbors,)
m = 3;
index = find_nn(xc,yc,m);

% pull out nearest neighbor values from y
y1 = y(index(:,1),1); % pulls out nearest neighbor crime rates

% plot crime rates in each neighborhood vs that in the nearest neighbor
plot(y,y1,'.g');
xlabel('neighborhood crime rates');
ylabel('nearest neighbor crime rates');
fprintf(1,'in pause mode, hit any key to continue \n');
pause;

y2 = y(index(:,1),1)+y(index(:,2),1); % crime rates in nearest 2 neighbors
y2 = y2/2; % an average of these

% plot crime rates in each neighborhood vs the average in the nearest 2 neighbors
plot(y,y2,'.r');
xlabel('neighborhood crime rates');
ylabel('average of 2 nearest neighbor crime rates');
