function Tfield
% Tfield  Load and plot temperature field stored in custom format text file
%
% Synopsis:  Tfield
%
% Input:     none
%
% Output:    2D color image with contour plot overlay

fin = fopen('Tfield.dat','r');
data = fscanf(fin,'%f');          %  Load *all* data into a single matrix

nx = data(1);   ny = data(2);     %  Number of x and y grid lines
lastx = 2 + nx;                   %  Ending index of xgrid values in ``data''
x = data(3:lastx)*100;            %  Grid lines perpendicular to x axis, convert from m to cm
lasty = lastx + ny;               %  Ending index of xgrid values in ``data''
y = data(lastx+1:lasty)*100;      %  Grid lines perpendicular to y axis, convert from m to cm

T = data(lasty+1:lasty+nx*ny);    %  Extract temperature data
T = reshape(T,nx,ny)';            %  Convert to T(xrows,ycols) then transpose to T(yrows,xcols)

close all                         %  Close any previously opened Figure windows
[XG,YG] = meshgrid(x,y);          %  Generate grid matrices
pcolor(XG,YG,T)                   %  Filled pseudocolor image of temperature field
shading interp                    %  Use interpolated, instead of blockwise, shading
% colormap('hot')
colormap('jet')                   %  Color map ranging from blue (low) to dark red (high)
set(gca,'TickDir','out');         %  Set axes so that ticks are outside

minT = round(min(min(T)));        %  Minimum temperature rounded to integer
maxT = round(max(max(T)));        %  Maximum temperature rounded to integer
hold on                           %  Add next plot to current figure, don't overwrite
intervals = minT:10:maxT;         %  temperature contours in 10 deg C intervals
contour(XG,YG,T,intervals,'k');   %  draw contour plot
hold off
