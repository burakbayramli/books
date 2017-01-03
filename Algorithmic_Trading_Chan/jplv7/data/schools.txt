% Ohio school district data set
% with proficiency scores, school district information
% latitude-longitude coordinates for district centroids

load schools.data;

% col 1 = school district ID
% col 2 = longitude centroid for the district
% col 3 = latitude centroid for the district
% col 4 = 2000 census pop
% --- >school district characteristics
% col 5 = Enrollment
% col 6 = Students with Disabilities
% col 7 = Students in District Less Than Half The Year
% col 8 = Students in Same School Less Than Half The Year
% col 9 = Students from Families in Ohio Works First
% col 10= Students from Eligible Households Approved to Receive Free and Reduced Price Lunch (%)
% col 11= Median Income
% col 12= "Average Number of Students Per Teacher (K-12)" "
% col 13= K-8 Teachers Certified in Their Teaching Area(All Courses)"
% col 14= "9-12 Teachers Certified in  Their Teaching Area(Core Courses)"
% col 15= "9-12 Teachers Certified in Their Teaching Area(All Courses)"
% col 16= Teacher Attendance Rate
% col 17= Local Revenue Per Pupil
% col 18= State Revenue Per Pupil
% col 19= Federal Revenue Per Pupil
% col 20= Total Revenue Per Pupil
% col 21= Per Pupil Spending on Instruction
% col 22= Per Pupil Spending on Building Operations
% col 23= Per Pupil Spending on Administration
% col 24= Per Pupil Spending on Pupil Support
% col 25= Per Pupil Spending on Staff Support
% col 26= Total Expenditures Per Pupil
% --->school district proficiency test results
% col 27= # students required to take 12th grade citizenship in 2000-01
% col 28= % below proficient 2000-01
% col 29= % proficient
% col 30= % advanced
% col 31= % below proficient 1999-2000
% col 32= % proficient
% col 33= % advanced
% col 34= % below proficient 1998-1999
% col 35= % proficient
% col 36= % advanced
% col 37= # students required to take 12th grade math in 2000-01
% col 38= % below proficient 2000-01
% col 39= % proficient
% col 40= % advanced
% col 41= % below proficient 1999-2000
% col 42= % proficient
% col 43= % advanced
% col 44= % below proficient 1998-1999
% col 45= % proficient
% col 46= % advanced
% col 47= # students required to take 12th grade reading in 2000-01
% col 48= % below proficient 2000-01
% col 49= % proficient
% col 50= % advanced
% col 51= % below proficient 1999-2000
% col 52= % proficient
% col 53= % advanced
% col 54= % below proficient 1998-1999
% col 55= % proficient
% col 56= % advanced
% col 57= # students required to take 12th grade writing in 2000-01
% col 58= % below proficient 2000-01
% col 59= % proficient
% col 60= % advanced
% col 61= % below proficient 1999-2000
% col 62= % proficient
% col 63= % advanced
% col 64= % below proficient 1998-1999
% col 65= % proficient
% col 66= % advanced
% col 67= % # students required to take 12th grade science in 2000-01
% col 68= % below proficient 2000-01
% col 69= % proficient
% col 70= % advanced
% col 71= % below proficient 1999-2000
% col 72= % proficient
% col 73= % advanced
% col 74= % below proficient 1998-1999
% col 75= % proficient
% col 76= % advanced

% long = schools(:,2);
% latt = schools(:,3);

% W = make_neighborsw(latt,long,6);

y = schools(:,26);  % total expenditures per pupil
n = length(y);
x0 = ones(n,1);     % constant term
x1 = schools(:,7);  % Students in District Less Than Half The Year
x2 = schools(:,10); % Students from Eligible Households Approved to Receive Free and Reduced Price Lunch (%)
x3 = schools(:,11); % Median Income
x4 = schools(:,12); % Average Number of Students Per Teacher (K-12)
x5 = schools(:,16); % Teacher Attendance Rate
x6 = schools(:,19); % Federal Revenue Per Pupil

x = [x0 x1 x2 x3 x4 x5 x6];
vnames = strvcat('exp per pupil','constant','student attr','free lunch','income', ...
                 'students/teacher','teacher attend','fed revenue');
result = ols(y,x);
prt(result,vnames);

% result2 = sar(y,x,W);
% prt(result2,vnames);
