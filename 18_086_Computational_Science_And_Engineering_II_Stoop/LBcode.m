% 2D Lattice Boltzmann (BGK) model of a fluid.
%  c4  c3   c2  D2Q9 model. At each timestep, particle densities propagate
%    \  |  /    outwards in the directions indicated in the figure. An
%  c5 -c9 - c1  equivalent 'equilibrium' density is found, and the densities
%    /  |  \    relax towards that state, in a proportion governed by omega.
%  c6  c7   c8      Iain Haslam, March 2006.
omega=1.0; 
density=1.0; 
t1=4/9; 
t2=1/9; 
t3=1/36; 
c_squ=1/3; 
nx=101; 
ny=101;
F=repmat(density/9,[nx ny 9]); 
FEQ=F; 
msize=nx*ny; 
CI=[0:msize:msize*7];

BOUND=rand(nx,ny)>0.97; %extremely porous random domain
ON=find(BOUND); %matrix offset of each Occupied Node
TO_REFLECT=[ON+CI(1) ON+CI(2) ON+CI(3) ON+CI(4) ...
            ON+CI(5) ON+CI(6) ON+CI(7) ON+CI(8)];
REFLECTED= [ON+CI(5) ON+CI(6) ON+CI(7) ON+CI(8) ...
            ON+CI(1) ON+CI(2) ON+CI(3) ON+CI(4)];
avu=1; 
prevavu=1; 
ts=0; 
deltaU=5e-1; 
numactivenodes=sum(sum(1-BOUND));

while (ts<4000 & 1e-10<abs((prevavu-avu)/avu)) | ts<100
    % Propagate
    F(:,:,4)=F([2:nx 1],[ny 1:ny-1],4);
    F(:,:,3)=F(:,[ny 1:ny-1],3);
    F(:,:,2)=F([nx 1:nx-1],[ny 1:ny-1],2);
    F(:,:,5)=F([2:nx 1],:,5);
    F(:,:,1)=F([nx 1:nx-1],:,1);
    F(:,:,6)=F([2:nx 1],[2:ny 1],6);
    F(:,:,7)=F(:,[2:ny 1],7); 
    F(:,:,8)=F([nx 1:nx-1],[2:ny 1],8);
    
    %Densities bouncing back at next timestep
    BOUNCEDBACK=F(TO_REFLECT); 
    % Calculate macroscopic densities and velocities
    DENSITY=sum(F,3);
    UX=(sum(F(:,:,[1 2 8]),3)-sum(F(:,:,[4 5 6]),3))./DENSITY;
    UY=(sum(F(:,:,[2 3 4]),3)-sum(F(:,:,[6 7 8]),3))./DENSITY;
    
    UX(1,1:ny)=UX(1,1:ny)+deltaU; %Increase inlet pressure
    UX(ON)=0; UY(ON)=0; DENSITY(ON)=0;
    U_SQU=UX.^2+UY.^2; 
    U_C2=UX+UY; 
    U_C4=-UX+UY; 
    U_C6=-U_C2; 
    U_C8=-U_C4;
    % Calculate equilibrium distribution: stationary
    FEQ(:,:,9)=t1*DENSITY.*(1-U_SQU/(2*c_squ));
    % nearest-neighbours
    FEQ(:,:,1)=t2*DENSITY.*(1+UX/c_squ+0.5*(UX/c_squ).^2-U_SQU/(2*c_squ));
    FEQ(:,:,3)=t2*DENSITY.*(1+UY/c_squ+0.5*(UY/c_squ).^2-U_SQU/(2*c_squ));
    FEQ(:,:,5)=t2*DENSITY.*(1-UX/c_squ+0.5*(UX/c_squ).^2-U_SQU/(2*c_squ));
    FEQ(:,:,7)=t2*DENSITY.*(1-UY/c_squ+0.5*(UY/c_squ).^2-U_SQU/(2*c_squ));
    % next-nearest neighbours
    FEQ(:,:,2)=t3*DENSITY.*(1+U_C2/c_squ+0.5*(U_C2/c_squ).^2-U_SQU/(2*c_squ));
    FEQ(:,:,4)=t3*DENSITY.*(1+U_C4/c_squ+0.5*(U_C4/c_squ).^2-U_SQU/(2*c_squ));
    FEQ(:,:,6)=t3*DENSITY.*(1+U_C6/c_squ+0.5*(U_C6/c_squ).^2-U_SQU/(2*c_squ));
    FEQ(:,:,8)=t3*DENSITY.*(1+U_C8/c_squ+0.5*(U_C8/c_squ).^2-U_SQU/(2*c_squ));
    F=omega*FEQ+(1-omega)*F;
    F(REFLECTED)=BOUNCEDBACK;
    prevavu=avu;avu=sum(sum(UX))/numactivenodes; ts=ts+1;
end

figure;colormap(gray(2));image(2-BOUND');hold on;
quiver(2:nx,1:ny,UX(2:nx,:)',UY(2:nx,:)');
title(['Flow field after ',num2str(ts),'\deltat']);xlabel('x');ylabel('y');
%Flow through random grid of cells