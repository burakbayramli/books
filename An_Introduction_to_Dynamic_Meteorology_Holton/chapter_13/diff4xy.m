function numdif = diff4xy(Dk4,Nx,Ny,U);
% Function for fourth order diffusion computation in x and y space.
% Periodic domain in x, bounded in y.
Nyl = Ny-1;
Nxl = Nx-1;
% Do smoothing in y space for 1st derivative zero at boundaries
numdif = zeros(size(U));          
numdif(4:Ny-3,:) = Dk4*(U(6:Ny-1,:) -4*U(5:Ny-2,:)+6*U(4:Ny-3,:)...
    -4*U(3:Ny-4,:)+U(2:Ny-5,:));
numdif(3,:) = Dk4*(-3*U(2,:) +6*U(3,:)-4*U(4,:)+U(5,:));
numdif(2,:) = Dk4*(2*U(2,:) -3*U(3,:) +U(4,:));
numdif(Ny-2,:) = Dk4*(-3*U(Nyl,:)+6*U(Ny-2,:)-4*U(Nyl-2,:)+U(Nyl-3,:));
numdif(Ny-1,:) = Dk4*(2*U(Nyl,:) -3*U(Nyl-1,:) + U(Nyl-2,:));
%do smoothing in x space with periodicity
numdif(:,3:Nxl-1) = numdif(:,3:Nxl-1)+Dk4*(U(:,5:Nx) -4*U(:,4:Nxl)+6*U(:,3:Nxl-1)...
    -4*U(:,2:Nxl-2)+U(:,1:Nxl-3));
numdif(:,2) = numdif(:,2)+Dk4*(U(:,4) -4*U(:,3)+6*U(:,2)...
    -4*U(:,1)+U(:,Nx));
numdif(:,1) = numdif(:,1)+Dk4*(U(:,3) -4*U(:,2)+6*U(:,1)...
    -4*U(:,Nx)+U(:,Nxl));
numdif(:,Nx) = numdif(:,1);
numdif(:,Nxl) = numdif(:,Nxl)+Dk4*(U(:,2) -4*U(:,1)+6*U(:,Nxl)...
    -4*U(:,Nxl-1)+U(:,Nxl-2));