function z=banana(parm,flag)
% PURPOSE: Banana function for testing optimization
% ----------------------------------------------
% USAGE: z = banana(parm,flag)
% where: parm = a 2 by 1 vector with
%               x,y input arguments
%        flag = 0 for z = 100*(y-x.^2).^2+(1-x).^2
%        flag = 1 for -z
% ----------------------------------------------
% RETURNS: z = 100*(y-x.^2).^2+(1-x).^2, if flag = 0
%          -z if flag = 1
% ----------------------------------------------

if nargin == 1
iflag = 0;
elseif nargin == 2
iflag = 1;
end;

[t1 t2] = size(parm);
if t2 == 1
x = parm(1,1);
y = parm(2,1);
else
x = parm(1,1);
y = parm(1,2);
end;

  z = 100*(y-x.^2).^2+(1-x).^2;
if iflag == 1
z = -z;
end;