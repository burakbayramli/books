% This function module a mesh of 8-nodded quadrilateral elements
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
%
     n1 = (i-1)*(3*NYE+2)+2*j - 1;
     n2 = i*(3*NYE+2)+j - NYE - 1;
     n3 = i*(3*NYE+2)+2*j-1; 
     n4 = n3 + 1;
     n5 = n3 + 2;
     n6 = n2 + 1;
     n7 = n1 + 2;
     n8 = n1 + 1;
 %
    geom(n1,:) = [(i-1)*dhx - X_origin    (j-1)*dhy - Y_origin ];
    geom(n3,:) = [i*dhx - X_origin       (j-1)*dhy - Y_origin  ]; 
    geom(n2,:) = [(geom(n1,1)+geom(n3,1))/2  (geom(n1,2)+geom(n3,2))/2];
    geom(n5,:) = [i*dhx- X_origin       j*dhy - Y_origin ]; 
    geom(n4,:) = [(geom(n3,1)+ geom(n5,1))/2  (geom(n3,2)+ geom(n5,2))/2];
    geom(n7,:) = [(i-1)*dhx - X_origin       j*dhy - Y_origin  ];
    geom(n6,:) = [(geom(n5,1)+ geom(n7,1))/2  (geom(n5,2)+ geom(n7,2))/2];    
    geom(n8,:) = [(geom(n1,1)+ geom(n7,1))/2  (geom(n1,2)+ geom(n7,2))/2];  
 %    
    nel = k;       
    nnd = n5;
    connec(k,:) = [n1 n2 n3 n4 n5 n6 n7 n8];
   end
end
%

