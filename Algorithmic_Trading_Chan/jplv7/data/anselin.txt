% A spatial dataset on crime, household income and housing values
% in 49 Columbus, Ohio neighborhoods
% from:  
% Anselin, L. 1988. Spatial Econometrics: Methods and Models,
% (Dorddrecht: Kluwer Academic Publishers).

% 5 columns:
% column1 = crime
% column2 = household income
% column3 = house values
% column4 = latitude coordinate
% column5 = longitude coordinate

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
n = length(anselin);
y = anselin(:,1);
x = [ones(n,1) anselin(:,2:3)]; 
latt = anselin(:,4);
long = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');


result = ols(y,x);
prt(result.vnames);
