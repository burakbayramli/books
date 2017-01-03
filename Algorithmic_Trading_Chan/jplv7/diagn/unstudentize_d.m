% PURPOSE: demonstrate studentized and unstudentize functions
%
%
% usage: unstudentize_d


% demonstrate case of a vector

x = 1:20;
xx = x';
xstud = studentize(xx);
xunstud = unstudentize(xstud,xx);
in.cnames = strvcat('raw vector','studentized','unstudentized');
mprint([xx,xstud,xunstud],in);

% demonstrate case of a matrix
x = 1:20;
xx = [x' x'.*x'];
xstud = studentize(xx);
xunstud = unstudentize(xstud,xx);

in.cnames = strvcat('x1raw','x2raw','x1stud','x2stud','x1unstud','x2unstud');
mprint([xx,xstud,xunstud],in);

