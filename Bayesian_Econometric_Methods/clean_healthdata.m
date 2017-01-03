%This m-file cleans the infant health/alcohol consumption data

clear;
clc;

load healthdata.txt;

drink = healthdata(:,1);
education = healthdata(:,5);
havekid_before = healthdata(:,7);
famincome = healthdata(:,8);
parent_drink = healthdata(:,9);
num_visit = healthdata(:,10);
healthprob  = healthdata(:,12);

%Create y and r variables;
nobs = length(drink);
r = 3*ones(nobs,1);
points1 = find(drink==1);
points2 = find(drink==2 | drink==3);
r(points1) = 1;
r(points2) = 2;

y = 3*ones(nobs,1);
points3 = find(num_visit==1);
points4 = find(num_visit==2 | num_visit==3);
y(points3) = 1;
y(points4) = 2;

%----------------
%Create Dummy label vector for R-outcome, upper and lower truncation
%regions
%---------------
D = zeros(nobs,3);
a_r = zeros(nobs,1); b_r = a_r;
for i = 1:nobs;
    D(i,r(i))=1;
    if r(i)==1;
        a_r(i) = -999; b_r(i) = 0;
    elseif r(i)==2;
        a_r(i) = 0; b_r(i) = 1;
    else
        a_r(i) = 1; b_r(i) = 999;
    end;
end;

%-------------------
%Create lower and upper truncation regions for y-outcome
%----------------
a_y = zeros(nobs,1); b_y = a_y;
for i = 1:nobs;
    if y(i)==1;
        a_y(i) = -999; b_y(i) = 0;
    elseif y(i)==2;
        a_y(i) = 0; b_y(i) = 1;
    else
        a_y(i) = 1; b_y(i) = 999;
    end;
end;
xy = [education havekid_before famincome healthprob];
xbary = [xy D];
xr = [ones(nobs,1) education famincome parent_drink];

save health_clean y r xbary xr a_r b_r a_y b_y;


