% Mesh2D for 1 element

function mesh2d;
include_flags;

lp = sqrt(nnp);             % number of nodes in the x and y 
x0 = linspace(0,2,lp);      % equal bisection of the x nodes

y0 = 0.5*x0/2;              % the bottom geometry line  

x = [];    
for i = 1:lp                
x = [x x0];                 % collect the x coordinates

y1 = linspace(y0(i),1,lp);  % bisection of y coodinates starting from a new location   
y(i:lp:lp*(lp-1)+i) = y1;   % collection of y coordinates to the appropriate location

end

% generate the IEN connectivity array
rowcount = 0;
for elementcount = 1:nel
    IEN(1,elementcount) = elementcount + rowcount;
    IEN(2,elementcount) = elementcount + 1 + rowcount;
    IEN(3,elementcount) = elementcount + (lp + 1) + rowcount;
    IEN(4,elementcount) = elementcount + (lp) + rowcount;
    if mod(elementcount,lp-1) == 0
        rowcount = rowcount + 1;
    end
end

% plot mesh and natural boundary conditions
plotmesh;




