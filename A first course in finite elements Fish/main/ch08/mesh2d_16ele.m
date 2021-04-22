% Mesh2D for 1 element

function mesh2d_16ele
include_flags;


% lp=5;
% x0 = linspace(0,2,lp);
% 
% y0 = 0.5*x0/2;
% 
% 
% x1 = []; y2 = zeros(1,nnp);
% for i = 1:lp
% x1 = [x1 x0];
% 
% y1 = linspace(y0(i),1,lp);
% y2(i:lp:lp*(lp-1)+i) = y1;
% 
% end
% x = x1;
% y = y2;


for node = 1:nnp    % loop over all nodes
     
% Node #, x coords
if (mod(node,5) == 1)
    x(node) = 0*(2/4);
elseif (mod(node,5) == 2)
    x(node) = 1*(2/4);
elseif (mod(node,5) == 3)
    x(node) = 2*(2/4);
elseif (mod(node,5) == 4)
    x(node) = 3*(2/4);
elseif (mod(node,5) == 0)
    x(node) = 4*(2/4);
end
 
% Node #, y coords
if ((node/5) <= 1)
    y(node) = 0 + (0.5/2)*((node-1)/2);
elseif ((1 < (node/5)) && ((node/5) <= 2))
    y(node) = 0.25 + ((0.5-(1/8))/2)*((node-6)/2);
elseif ((2 < (node/5)) && ((node/5) <= 3))
    y(node) = 0.5 + ((0.5-(2/8))/2)*((node-11)/2);
elseif ((3 < (node/5)) && ((node/5) <= 4))
    y(node) = 0.75 + ((0.5-(3/8))/2)*((node-16)/2);
elseif ((4 < (node/5)) && ((node/5) <= 5))
    y(node) = 1 + ((0.5-(4/8))/2)*((node-21)/2);
end
 
end
% Element #, connectivity
rowcount = 0;
for elementcount = 1:nel
    IEN(1,elementcount) = elementcount + rowcount;
    IEN(2,elementcount) = elementcount + 1 + rowcount;
    IEN(3,elementcount) = elementcount + 6 + rowcount;
    IEN(4,elementcount) = elementcount + 5 + rowcount;
    if mod(elementcount,4) == 0
        rowcount = rowcount + 1;
    end
end




plotmesh;




