% This module generates a mesh of linear quadrilateral elements
%
global nnd nel nne  nodof eldof  n 
global geom connec dee nf Nodal_loads
global Length Width NXE NYE X_origin Y_origin dhx dhy 
%
%
nnd = 0;
k = 0;
for i = 1:NXE
   for j=1:NYE
        k = k + 1;
        n1 = j + (i-1)*(NYE + 1);
        geom(n1,:) = [(i-1)*dhx - X_origin    (j-1)*dhy - Y_origin ];
        n2 = j + i*(NYE+1);
        geom(n2,:) = [i*dhx - X_origin       (j-1)*dhy - Y_origin  ];
        n3 = n1 + 1;
        geom(n3,:) = [(i-1)*dhx - X_origin       j*dhy - Y_origin  ];
        n4 = n2 + 1;
        geom(n4,:) = [i*dhx- X_origin       j*dhy - Y_origin       ];
        nel = k;
        connec(nel,:) = [n1  n2  n4  n3];
        nnd = n4;
        end
end
%

