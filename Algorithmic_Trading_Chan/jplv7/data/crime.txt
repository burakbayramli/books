
% 47 states crime data from: I. Ehrlich (1973)
% Participation in illegitimate activities: a theoretical
% and empirical investigation (journal of political economy)

% rows = states
% columns = 
% 1) crime, 1960 
% 2) percent of males aged 14-24
% 3) south, dummy variable
% 4) mean years of schooling
% 5) police expenditures in 1960
% 6) police expenditures in 1959
% 7) labor force participation rate
% 8) sex, number of males per 1000 females
% 9) state population
% 10) nonwhites per 1000 population
% 11) unemployment rate of urban males 14-24 
% 12) unemployment rate of urban males 35-39
% 13) wealth
% 14) income inequality
% 15) probability of imprisionment
% 16) average time served in state prisons


load crime.dat;
y = crime(:,1);
[n k] = size(crime);
x = [ones(n,1) crime(:,2:end)];

vnames = strvcat('crime','constant','male14-24','south','school','police60','police59', ...
'labor','sex','pop','nonwhite','unemp young','unemp old','wealth', ...
'inequality','imprisioned','timeserved');

result = ols(y,x);
prt(result,vnames);

