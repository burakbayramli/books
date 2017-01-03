function demoPlaneFit
% demoPlaneFit  Least squares fit to a plane: z = c(1)*x + c(2)*y + c(3)
%
% Synopsis:  demoPlaneFit
%
% Input: none, data is defined as constants in the code
%
% Output:  Two 3D plots of data and the plane fit to the data 

x = [-4     -3    -2      -1     0       1     2      3      4   ]';
y = [ 4     -3     5      -4     1      -3     4     -1      3   ]';
z = [18.74  -1.10  19.88  -5.71  6.20  -10.37  4.96  -5.30   1.54]';

A = [x  y  ones(size(x))];       %  matrix of overdetermined system
c = A\z                          %  Least squares solution

% --- Draw data and plane with default camera position (MATLAB computes) 
figureSize([3.5 2.5],0.92)
subplot(1,2,1)
drawData(c,x,y,z)
title('Default view angle');
set(gca,'CameraViewAngleMode','manual')   %  prevent auto adjusting aspect ratio  

% --- Redraw data and set camera target and position to view along
%     the edge of the least squares plane plane
%     See "Using MATLAB Graphics", p. 3-38, version 5.2
subplot(1,2,2)
drawData(c,x,y,z)
title('Move camera target and zoom out');
set(gca,'CameraViewAngleMode','manual')   %  prevent auto adjusting aspect ratio  

% -- Set target to middle of the plane
ctarg = [mean(x) mean(y) 0]; 
ctarg(3) = c(1)*ctarg(1) + c(2)*ctarg(2) + c(3);
set(gca,'CameraTarget',ctarg);
% -- Querey current camera position;  keep x and y, replace z with value
%    that lies in the plane
cpos = get(gca,'CameraPosition');
cpos(3) = c(1)*cpos(1) + c(2)*cpos(2) + c(3);
% -- Adjust camera position by zooming out.  See p. 3-38 in Using MATLAB Graphics
dist = -.25;                               %  Scaling factor for zoom
newPos = cpos - dist*(cpos - ctarg);       %  New position is zoomed out
set(gca,'CameraPosition',newPos);

% ==============
function drawData(c,x,y,z)
% drawData  Utility function to draw discrete data and the plane
drawPlane(c,[min(x) max(x)  min(y)  max(y)])
hold on
plot3(x,y,z,'.','MarkerSize',12);
xlabel('x');  ylabel('y');
