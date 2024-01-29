clear all;

h=0.025;
[X,Y] = meshgrid(0:h:5,0:h:5);  % generate a square mesh grid
Z=sqrt(X.^2 + (Y-3).^2);        % Z value of the profile 

levels=20;                           % number of levels
N=size(X,1);      
load mymap.dat;                      % load my own colormap 
interval=min(levels, size(mymap,1)); % Z value interval for each color
low=-abs(max(max(Z)))/interval;  % Z value for the first color in the colormap

% next block: set the value of Z inside and outside of the quarter circle
for i=1:N
  for j=1:N
    if (X(i,j)^2 + Y(i,j)^2) < 4
      Z(i,j)=low;     % set Z=low if inside the quarter circle
    end
  end
end

colormap(mymap);     %  sets the colormap to the matrix "mymap"
contourf(X,Y,Z,levels);