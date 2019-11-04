function vnew=tangent_directions(x,h,v)
% TANGENT_DIRECTIONS
%
% This is an example of a way to add new directions. 
%
% If any point in the stencil does not satisfy the linear constraints,
% I will add tangent directions to the stencil.
%
% The linear constraints, which we handle with the extreme barrier
% method, are x(1) + x(2)  >= 1
%
vnew=[];
[mv,nv]=size(v);
yesno=1;
for i=1:nv
    x_trial=x + h * v(:,i);
    yesno=yesno*test_constraint(x_trial);
end
if yesno==0
   vnew=zeros(2,2);
%
% The linear constraints are (1, 1)^T x >= 1.
% So a tangent vector is  (-1, 1)^T. We do not have to normalize
% this vector because imfil_core will do that.
%
   vnew(:,1)=[-1,1]';
   vnew(:,2)=-vnew(:,1);
end


function yesno=test_constraint(x)
%
% No deep thinking here. Either the constraint is violated (yesno = 0) or
% it's not (yesno = 1).
%
val = x(1) + x(2) ;
yesno = (val >= 1);
