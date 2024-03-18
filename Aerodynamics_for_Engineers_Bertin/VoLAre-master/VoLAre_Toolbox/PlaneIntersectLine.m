% Anthony Ricciardi
% 
function rOut=PlaneIntersectLine(rPlane,nvecPlane,rLine,tanvLine)
%% Plane Defined by postion and vector normal to plane
% %postion vector
% rPlane = [10 0 0]; %ex
% %
% %normal vector (unit) 
% nvecPlane = [1 0 0]; %ex
% %
% %% Line Defined by postion and unit tangent vector
% rLine = [ 0 0 1];
% tanvLine = [ 1 1 0];
%
%
%

Stiff = [ 
    nvecPlane(1) nvecPlane(2) nvecPlane(3) 0;
    1 0 0 -tanvLine(1);
    0 1 0 -tanvLine(2);
    0 0 1 -tanvLine(3)];
%
Forcing = [
    nvecPlane'*rPlane;
    repmat(rLine,[1,size(rPlane,2)])];
%
%
% Solve Linear System of Equations
state = Stiff\Forcing;
rOut=state(1:3,:);