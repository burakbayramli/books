
% love.dat documentation

% 39 MBA students in a class at U of Chicago
% From George and McCullogh (1993 JASA)
% y = happiness (column 1), 
%               10 = happy, 1 = suicidal
% x1 = money    (column 2)
%      family income in thousands
% x2 = sex,     (column 3)
%      0,1 with 1=satisfactory
% x3 = love     (column 4)
%       1,2,3 with 3 = love, 1 = lonely
% x4 = work,    (column 4)
%            1 = seeking other work
%            5 = job enjoyable

load love.dat;
[n nvar] = size(love);
y = love(:,1); 
x = [ones(n,1) love(:,2:nvar)];

result = ols(y,x);

vnames = strvcat('happiness','constant','money','sex','love','work');

prt(result,vnames);
