% Mesh2D for 1 element

function mesh2d_64ele
include_flags;




for node = 1:nnp    % loop over all nodes
  
% Node #, x coords
if (mod(node,9) == 1)
    x(node) = 0*(2/8);
elseif (mod(node,9) == 2)
    x(node) = 1*(2/8);
elseif (mod(node,9) == 3)
    x(node) = 2*(2/8);
elseif (mod(node,9) == 4)
    x(node) = 3*(2/8);
elseif (mod(node,9) == 5)
    x(node) = 4*(2/8);
elseif (mod(node,9) == 6)
    x(node) = 5*(2/8);
elseif (mod(node,9) == 7)
    x(node) = 6*(2/8);
elseif (mod(node,9) == 8)
    x(node) = 7*(2/8);
elseif (mod(node,9) == 0)
    x(node) = 8*(2/8);
end
 
% Node #, y coords
if ((node/9) <= 1)
    y(node) = (0/8) + ((8-0)/16)*((node-1)/8);
elseif ((1 < (node/9)) && ((node/9) <= 2))
    y(node) = (1/8) + ((8-1)/16)*((node-10)/8);
elseif ((2 < (node/9)) && ((node/9) <= 3))
    y(node) = (2/8) + ((8-2)/16)*((node-19)/8);
elseif ((3 < (node/9)) && ((node/9) <= 4))
    y(node) = (3/8) + ((8-3)/16)*((node-28)/8);
elseif ((4 < (node/9)) && ((node/9) <= 5))
    y(node) = (4/8) + ((8-4)/16)*((node-37)/8);
elseif ((5 < (node/9)) && ((node/9) <= 6))
    y(node) = (5/8) + ((8-5)/16)*((node-46)/8);
elseif ((6 < (node/9)) && ((node/9) <= 7))
    y(node) = (6/8) + ((8-6)/16)*((node-55)/8);
elseif ((7 < (node/9)) && ((node/9) <= 8))
    y(node) = (7/8) + ((8-7)/16)*((node-64)/8);
elseif ((8 < (node/9)) && ((node/9) <= 9))
    y(node) = (8/8) + ((8-8)/16)*((node-73)/8);
end
 
end
 
% Element #, connectivity
rowcount = 0;
for elementcount = 1:nel
    IEN(1,elementcount) = elementcount + rowcount;
    IEN(2,elementcount) = elementcount + 1 + rowcount;
    IEN(3,elementcount) = elementcount + 10 + rowcount;
    IEN(4,elementcount) = elementcount + 9 + rowcount;
    if mod(elementcount,8) == 0
        rowcount = rowcount + 1;
    end
end


plotmesh;


fprintf(1,'  Mesh Params \n');
fprintf(1,'--------------- \n');
fprintf(1,'No. of Elements  %d \n',nel);
fprintf(1,'No. of Nodes     %d \n',nnp);
fprintf(1,'No. of Equations %d \n\n',neq);


