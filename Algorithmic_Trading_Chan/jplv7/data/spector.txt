%       (data from Spector and Mazzeo, 1980)
% data on 32 students TUCE scores
% 5 columns with rows = students
% 1) post grade
% 2) constant term
% 3) psi
% 4) tuce (test of understanding of college economics) score
% 5) grade point average

load spector.dat;
y = spector(:,1);
x = spector(:,2:5);

vnames=strvcat('grade','constant','psi','tuce','gpa');

res = probit(y,x);
prt(res,vnames);
