% data set on 1,965 Ohio Elementary School buildings for 2001-02 year

clear all;
load ohioschool.dat;
% variables are:
% col 1 = zip code
% col 2 = lattitude (zip centroid)
% col 3 = longitude (zip centroid)
% col 4 = buidling irn
% col 5 = district irn
% col 6 = # of teachers (FTE 2001-02)
% col 7 = teacher attendance rate
% col 8 = avg years of teaching experience
% col 9 = avg teacher salary
% col 10 = Per Pupil Spending on Instruction
% col 11 = Per Pupil Spending on Building Operations
% col 12 = Per Pupil Spending on Administration
% col 13 = Per Pupil Spending on Pupil Support
% col 14 = Per Pupil Spending on Staff Support
% col 15 = Total Expenditures Per Pupil
% col 16 = Per Pupil Spending on Instruction % of Total Spending Per Pupil
% col 17 = Per Pupil Spending on Building Operations % of Total Spending Per Pupil
% col 18 = Per Pupil Spending on Administration % of Total Spending Per Pupil
% col 19 = Per Pupil Spending on Pupil Support % of Total Spending Per Pupil
% col 20 = Per Pupil Spending on Staff Support % of Total Spending Per Pupil
% col 21 = irn number
% col 22 = avg of all 4th grade proficiency scores
% col 23 = median of 4th grade prof scores
% col 24 = building enrollment
% col 25 = short-term students < 6 months
% col 26 = 4th Grade (or 9th grade) Citizenship % Passed 2001-2002
% col 27 = 4th Grade (or 9th grade)  math % Passed 2001-2002
% col 28 = 4th Grade (or 9th grade)  reading % Passed 2001-2002
% col 29 = 4th Grade (or 9th grade)  writing % Passed 2001-2002
% col 30 = 4th Grade (or 9th grade)  science % Passed 2001-2002
% col 31 = pincome per capita income in the zip code area
% col 32 = nonwhite percent of population that is non-white
% col 33 = poverty percent of population in poverty
% col 34 = samehouse % percent of population living in same house 5 years ago
% col 35 = public % of population attending public schools
% col 36 = highschool graduates, educ attainment for 25 years plus
% col 37 = associate degrees, educ attainment for 25 years plus
% col 38 = college, educ attainment for 25 years plus
% col 39 = graduate, educ attainment for 25 years plus
% col 40 = professional, educ attainment for 25 years plus


ohio = ohioschool;

% fix a database problem
enroll = ohio(:,24);
teachers = ohio(:,6);
ppupil = (enroll./teachers); % pupils per teacher ratio
% find and pitch 2 observation where pupil/teacher ratio > 50
ind = find(ppupil < 50.0);
data = ohio(ind,:);
% pitch negative spending per pupil cases
ind = find(data(:,10) > 0 );
datag = data(ind,:);
latt = datag(:,2);
long = datag(:,3);

pscore = datag(:,23); % median proficiency score percentage passing
mscore = datag(:,22); % mean proficiency score
y = log(pscore);
nobs = length(y);


experience = data(:,8); % don't log since there are zeros, and it looks normal
salary = log(data(:,9));% log-transformation
enroll = data(:,24);    % reconstruct pupil/teacher ratios using good data
teachers = data(:,6);
ppupil = log(enroll./teachers); % pupils per teacher ratio

pinstruct = (data(:,10)/1000); % per pupil spending variables
pbuilding = (data(:,11)/1000);
padminist = (data(:,12)/1000);
ppsupport = (data(:,13)/1000);
pssupport = (data(:,14)/1000);

instructp = data(:,16); % percentage spending on each category variables, don't log these
buildingp = data(:,17); % they look pretty normally distributed
administp = data(:,18);
psupportp = data(:,19);
ssupportp = data(:,20);


% census info
pincome = log(data(:,31));
nonwhite = data(:,32);  % don't log since there are zeros
poverty = data(:,33);   % don't log since there are zeros
samehouse = data(:,34); % don't log, it looks more normal this way
public = data(:,35);    % don't log, since there are zeros
% educ attainment variables
highsch = data(:,36);
assoc = data(:,37);
college = data(:,38);
grad = data(:,39);
prof = data(:,40);



building = [pinstruct pbuilding padminist ppsupport pssupport instructp buildingp ...
            administp psupportp ssupportp experience salary ppupil];

census = [pincome nonwhite poverty samehouse public highsch assoc college grad prof];

x = [ones(nobs,1) building census];

vnames = strvcat('median proficiency','constant','pinstruction','pbuilding','padminist','ppsupport','pssupport', ...
'per_instruction','per_building','per_admin','per_pupil','per_staff','experience','salary','pupil/teacher', ...
'income','nonwhite','poverty','samehouse','public school','highschool','assoc','college','grad','professional');

result = ols(y,x);
prt(result,vnames);
