% average house values for 98 census tracts in Toledo, Ohio
% along with 13 explanatory variables
% and lattitude-longitude coordinates

% Data are sorted by distance from central city

% col1 = average house values for 98 census tracts
% col2 = census tract
% col3 = neighborhood
%        1=improving
%        2=static
%        3=declining
%        4=blighted
% col4 = net lot square foot
% col5 = total square foot living area
% col6 = family room
%        1=vacant lot
%        2=yes
%        3=no
% col7 = rec room
%        1=vacant lot
%        2=type1
%        3=type2
%        4=type3
%        5=type4
%        6=no rec room
% col8 = air conditioning
%        1=vacant lot
%        2=yes
%        3=no
% col9 = baths
%        1=vacant lot
%        2=no plumbing
%        3=water only
%        4=half bath
%        5=1 bath
%        6=1+1half 
%        7=1+2half
%        8=2
%        9=2+1half
%        10=2+2half
%        11=3
%        12=3+1half
%        13=3+2half
%        14=4
%        15=5
%        16=6
%        17= >6 baths
% col10 = relative condition
%        1=vacant lot
%        2=excellent
%        3=very good
%        4=good
%        5=average
%        6=fair
%        7=poor
%        8=very poor
%        9=Us
%        10=sv
% col11 = garage condition
%        1=vacant lot
%        2=excellent
%        3=very good
%        4=good
%        5=average
%        6=fair
%        7=poor
%        8=very poor
%        9=Us
%        10=sv
%        11=no garage
% col12 = value of land
% col13 = value of building
% col14 = year built
% col15 = lattitude
% col16 = longitude

load htoledo.dat;
y = htoledo(:,1);
n = length(y);
x = [ones(n,1) htoledo(:,3:11) htoledo(:,14)]; 
% Note: we skip census tract number in column 2
%       and land plus building value which add to y-variable
% convert year built to house age
maxyear = max(htoledo(:,14));
houseage = maxyear  - htoledo(:,14);
x(:,end) = houseage;

latt = htoledo(:,15);
long = htoledo(:,16);

vnames = strvcat('value','constant','neighborhood','lot sqft', ...
'total sqft','family room','rec room','air cond','baths','condition', ...
'garage condition','house age');

result = ols(y,x);
prt(result,vnames);

