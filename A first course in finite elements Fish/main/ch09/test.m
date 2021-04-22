% test 
% nonlinear Newton iteration to find the transformation of nodes from
% physical domain to natural doamin



clear all
close all


% node coord
%x = [2 4 5 3];
%y = [1 1 2 2];
x = [2 4 5 3];
y = [1 1.5 2.5 2];
xp = 4; yp = 1.8;

line([x x(1)],[y y(1)]); hold on;
plot(xp,yp,'*');


% shape functions
        



  
XY  = [x y]';

XYP = [xp yp]';
i = 0;

psi_p = 1;  eta_p = 1; 
EP  = [psi_p  eta_p]';
normR = 1;
while normR > 1e-8    
%    eta___psi= [psi_p  eta_p]
    
    N  = 0.25*[(1-psi_p)*(1-eta_p)  (1+psi_p)*(1-eta_p)  (1+psi_p)*(1+eta_p)  (1-psi_p)*(1+eta_p)]; % shape functions
    
    NN = [N            zeros(1,4);
          zeros(1,4)        N];

      
    r =  NN*XY - XYP;
    
    GN    = 0.25*[eta_p-1  1-eta_p   1+eta_p   -eta_p-1;
                 psi_p-1  -psi_p-1  1+psi_p    1-psi_p];

            
    J     = [GN*x' GN*y'];
    
    dEP =  -J \ r;

    EP = EP + dEP;
    
    psi_p = EP(1);
    eta_p = EP(2);

    
    normR = norm(r);
    i = i + 1;
    
    
end
fprintf(1,'Number of iteration to converge %d \n',i);
fprintf(1,'psi=%f      eta=%f \n',psi_p,eta_p);



psi  =  [-1 1 1 -1];
eta  =  [-1 -1 1 1];

plot([psi psi(1)],[eta eta(1)],'r'); hold on;
plot(psi_p,eta_p,'r*');
title('Reverse Transformation to find point in natural coordinates');
xlabel('x'); ylabel('y');
axis equal



