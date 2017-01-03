% sourceSinkPlot  Surface and contour plots of source/sink pair

ngrid = 40;                      %  Number of grid lines
xg = linspace(-5,5,ngrid);       %  Vector of both x and y grid locations
[X,Y] = meshgrid(xg,xg);         %  Prepare for vectorized R calculations

gam = 10;                        %  Strength of the source and sink
a = 1.5;                         %  Distance from origin to source/sink
Rsi   = sqrt( (X+a).^2 + Y.^2 );           %  Distance from sink
Rso = sqrt( (X-a).^2 + Y.^2 );             %  Distance from source
phi = (gam/(2*pi))*(log(Rsi) - log(Rso));  %  Potential function

subplot(2,1,1)                   %  First subplot for surface plot
surf(X,Y,phi)                    %  Create surface plot
view(-20,15)                     %  Adjust the viewing angle

subplot(2,1,2)                   %  Second surface plot for contours
levels = -3:3;                   %  Contour levels to be drawn
cs = contour(xg,xg,phi,levels);  %  Create contours
clabel(cs,levels);               %  and label them

colormap('gray')
