
% Data on 610 Ohio school districts from the Ohio Department of Education

% nobs = 610
% Variable definitions
% number of students in district           = school(1:nobs,1); 
% herfindahl index of concentration        = school(1:nobs,2); 
% unemployment rate                        = school(1:nobs,3); 
% percentage non-white                     = school(1:nobs,4); 
% median income                            = school(1:nobs,5);
% proportion of students on welfare        = school(1:nobs,6);
% expenditures per pupil                   = school(1:nobs,7);
% classize                                 = school(1:nobs,8);
% teacher salary                           = school(1:nobs,9);
% attendance of students                   = school(1:nobs,10);
% passed on all9th grade proficiency tests = school(1:nobs,11);
% attendence of staff                      = school(1:nobs,12);
% big city                                 = school(1:nobs,13);
% small city                               = school(1:nobs,14);
% suburban                                 = school(1:nobs,15);
% southeast region                         = school(1:nobs,16);
% central region                           = school(1:nobs,17);
% northcentral region                      = school(1:nobs,18);
% northeast region                         = school(1:nobs,19);
% northwest region                         = school(1:nobs,20);
% southcentral region                      = school(1:nobs,21);
% southwest region                         = school(1:nobs,22);
% dropout rate                             = school(1:nobs,23);

vnames = strvcat('passed 9th','#students','concentration','unemp','nonwhite', ...
'income','poor','expend/pupil','classize','teach salary','stud attend', ...
'staff attend','big','small','suburban','southeast','central','ncentral',...
'northeast','northwest','southcentral','southwest','dropout rate');


load school.dat;
% NOTE: there are reporting errors in staff attendance rates
%       the following adjusts these
ao = find(school(:,12) < 25); % adjust for staff attendance reporting errors
amean = mean(school(:,12));   % by setting equal to the mean
school(ao,12) = amean;

% variables 13-15 are dummy variables for big, small and suburban
% with rural left out

% variables 16-22 are dummy variables for regions of the state
% with 1 region left out

y = school(:,11);
x = [school(:,1:10) school(:,12:23)];

result =  ols(y,x);
prt(result,vnames);


