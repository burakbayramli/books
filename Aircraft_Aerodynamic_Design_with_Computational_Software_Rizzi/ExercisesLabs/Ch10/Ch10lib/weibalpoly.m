function [iner,area,cg] = weibalpoly(xyz)
% computes
% inertia tensor iner: int x_i x_j da
% area:                area
% cg:                  1/area int xi da
% for polygonal lamina in xyz(1:n,3)
% points assumed ordered along edge.
%-----------------------------------
n = size(xyz,1);
cgtmp = mean(xyz);
xyz  = xyz-ones(n,1)*cgtmp;
% make sure is closed. possible zero length edge does no harm
xyz  = [xyz;xyz(1,:)]; 
area =0;
iner = zeros(3,3);
cg   = zeros(1,3);
p0   = xyz(1,:);
for k = 1:n
    p1 = xyz(k+1,:);
    % triangle formed by (0,0), p0, and p1
    da   = 0.5*norm([p0(2)*p1(3)-p0(3)*p1(2),p0(3)*p1(1)-p0(1)*p1(3),p0(1)*p1(2)-p0(2)*p1(1)]);
    area = area + da;
    cgt  = 1/3*(p0+p1);
    cg   = cg+da*cgt;
    diner = da/12*((p0*p0' + p1*p1'+ p0*p1')*eye(3) ...
                  - p0'*p0 - p1'*p1 - 0.5*(p0'*p1 + p1'*p0));
    iner  = iner + diner;
    p0 = p1;
end
% final cg
cg = cg/area + cgtmp;
%parallel axis theorem
iner = iner+area*(cgtmp-cg)'*(cgtmp-cg);