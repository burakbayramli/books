% data on 25,357 single family homes sold in Lucas County, Ohio, 1993-1998
% from the county auditor
% 24 columns of information

% col1 = price, selling price
% col2 = yrbuilt, year built
% col3 = stories code, 1=one, 2=bilevel,3=multilvl,4=one+half,5=two,6=two+half,7=three
% col4 = TLA, total living area in square feet
% col5 = Wall code, 1=stucdrvt,2=ccbtile,3=metlvnyl,4=brick,5=stone,6=wood,7=partbrk
% col6 = #beds,      # of bedrooms
% col7 = #baths,     # of full bathrooms
% col8 = #halfbaths, # of half bathrooms
% col9 = frontage,  frontage in feet
% col10 = depth,     depth in feet
% col11 = garage type code,0=no garage,1=basement,2=attached,3=detached,4=carport
% col12 = garage sqft, garage size in square feet, or zero for no garage
% col13 = #rooms,    # of rooms
% col14 = lotsize,   lotsize in square feet
% col15 = sale date  sale date in format, yymmdd, e.g., Oct 17, 1997 = 971017
% col16 = assessed value, auditor's assessed value
% col17 = longitude,  location
% col18 = latitude,   location
% col19 = sold93, a year of sale dummy, 1=1993
% col20 = sold94, 1=1994
% col21 = sold95, etc.
% col22 = sold96
% col23 = sold97
% col24 = sold98

load house.dat;
y = log(house(:,1)); % log of the selling price
n = length(y);       % # of observations

% create explanatory variables matrix

yearblt = house(:,2);
age = 1999 - yearblt; % age of the house in years
age = age/100;

lotsize = log(house(:,14));
rooms = house(:,13);
bedrooms = house(:,6);
livinga = log(house(:,4));

x = zeros(n,8);        % an explanatory variables matrix

x(:,1) = ones(n,1);       % an intercept term
x(:,2) = age;             % house age
x(:,3) = age.*age;        % house age-squared
x(:,4) = age.*age.*age;   % house age-cubed
x(:,5) = lotsize;         % log of the house lotsize
x(:,6) = rooms;           % the # of rooms
x(:,7) = livinga;         % log of the total living area in the house
x(:,8) = bedrooms;        % the # of bedrooms

vnames = strvcat('log(price)','constant','age','age2','age3','lotsize','rooms','tla','beds');

result0 = ols(y,x);   % ols() is a toolbox function for least-squares estimation
prt(result0,vnames);  % print results using prt() toolbox function
