%==========================================================================
% Fluid-Structure Interaction - An Introduction to Finite Element Coupling
% (c) Jean-François SIGRIST, 2015
%--------------------------------------------------------------------------
% Example #3 - Bending Modes of an Elastic Plated Coupled to a Fluid Cavity
% Inertial Coupling
% (Chapter #4)
% Boundary conditions
% - Structure: clamped end at z=0, free end at z=L
% - Fluid: null pressure gradient at r=R,r=R' and z=0,z=L
% Compatibility conditions
% - Structure: null mean displacement u(z) on [0,L]
% - Fluid: null mean pressure p(r,z) on [R,R'],[0,L]
% Displacement-based formulation with condensation of pressure and Lagrange
% multipliers
%==========================================================================

% Initialisation
%---------------
clear all;
close all;

% Material & geometrical properties
L = 1;
R = 0.5;
mu = 7800;
nu = 0.3;
rho = 1000;
h = 0.1;
E = 2.1e11;
S = pi*(R^2-(R-h)^2);% Cross-section area
I = pi*(R^4-(R-h)^4)/4;% Cross-section inertia

% Compatibility condition
Compatibility = 'With';
%Compatibility = 'Without';


% Matrices
%---------

% 2D mesh
Jr = 10;
for j = 1:1:Jr+1
    r(j,1) = (j-1)*R/Jr;
end% for j = 1:1:Jr+1
Jz = 20;
for j = 1:1:Jz+1
    z(j,1) = (j-1)*L/Jz;
end% for j = 1:1:Jz+1

% Assembling structure matrices Ms and Ks
Id = eye(4);
Ms = zeros(2*(Jz+1),2*(Jz+1));
Ks = zeros(2*(Jz+1),2*(Jz+1));

for j=1:Jz
   % Element length
   lj = z(j+1)-z(j);
   % Elementary mass matrix
   msj = mu*S*lj*[13/35 11*lj/210 9/70 -13*lj/420; ...
                11*lj/210 lj^2/105 13*lj/420 -lj^2/140; ...
                9/70 13*lj/420 13/35 -11*lj/210 ; ...
                -13*lj/420 -lj^2/140 -11*lj/210 lj^2/105]; 
    % Elementary stiffness matrix
    ksj = E*I*[12/lj^3 6/lj^2 -12/lj^3 6/lj^2; ...
            6/lj^2 4/lj -6/lj^2 2/lj ; ...
           -12/lj^3 -6/lj^2 12/lj^3 -6/lj^2; ...
            6/lj^2 2/lj -6/lj^2 4/lj];
   % Localisation matrices 
   Lj=zeros(4,2*(Jz+1));
   Lj(1:4,2*j-1:2*j+2)=Id;
   % Mass and stiffness matrices
   Ks = Ks + Lj'*ksj*Lj;
   Ms = Ms + Lj'*msj*Lj;
end% for j=1:Jz

% Assembling fluid matrices Mf and Kf
e = [-1 1 1 -1]';
n = [-1 -1 1 1]';
Id = ones(4,1);
Mf = zeros((Jz+1)*(Jr+1),(Jz+1)*(Jr+1));
Kf = zeros((Jz+1)*(Jr+1),(Jz+1)*(Jr+1));

for j = 1:Jr
   % Element length
   aj  = (r(j+1)-r(j))/2;
   % Element center
   r0 = (r(j+1)+r(j))/2;
   for i = 1:1:Jz
      % Element height 
      bi = (z(i+1)-z(i))/2;
      % Elementary mass matrix
      mfij = aj*bi/4*(1+e*e'/3).*(1+n*n'/3);
      % Elementary stiffness matrix
      kfij = aj*bi/4*(((e*e')/aj^2.*(1+n*n'/3)) + ((n*n')/bi^2.*(1+e*e'/3)));
      % Localisation matrices
      Lij=zeros(4,(Jz+1)*(Jr+1));
      Lij(1,i   + (j-1)*(Jz+1)) = 1;
      Lij(2,i   + j*(Jz+1))     = 1;
      Lij(3,i+1 + j*(Jz+1))     = 1;
      Lij(4,i+1 + (j-1)*(Jz+1)) = 1;  
      % Mass and stiffness matrices
      Mf = Mf + Lij'*mfij*Lij;
      Kf = Kf + Lij'*kfij*Lij;
   end% for i = 1:1:Jz
end% for j = 1:Jr

% Assembling fluid-structure coupling matrix Rfs
Id = eye(4);
Rfs = zeros((Jz+1)*(Jr+1),2*(Jz+1));
for j =1:Jz
   % Element length
   lj = z(j+1)-z(j);
   % Elementary fluid-structure coupling matrix
   rfsj = [ 7*lj/20 lj^2/20 3*lj/20 -lj^2/30 ;...
           0      0       0          0     ;...
           0      0       0          0     ;...
           3*lj/20 lj^2/30 7*lj/20 -lj^2/30   ];
   % Localisation matrices
   Ljs = zeros(4,2*(Jz+1));
   Ljs(1:4,2*j-1:2*j+2) = Id;
   Ljf=zeros(4,(Jz+1)*(Jr+1));
   Ljf(1,j) = 1;
   Ljf(2,j+(Jz+1)) = 1;
   Ljf(3,j+1+(Jz+1)) = 1;
   Ljf(4,j+1) = 1;
   % Coupling matrix
   Rfs = Rfs +Ljf'*rfsj*Ljs;
end% for j =1:Jz

% Assembling matrices with boundary and compatibility conditions
% Fluid problem
Lf=ones(1,(Jr+1)*(Jz+1))*Mf;  
Kf = [Kf -Lf';-Lf 0];
Rfs = [Rfs', zeros(2*(Jz+1),1)]';
Mfs = rho*Rfs'*inv(Kf)*Rfs;

% Structure problem
switch Compatibility
    case 'With'
        disp('Calculation WITH compatibility condition for the structure')
        Ls = zeros(3,2*(Jz+1));
        Ls(1,1) = 1;
        Ls(2,2) = 1;
        Ls(3,1:2*(Jz+1)) = ones(1,2*(Jz+1))*Ms/mu/h;
        Ko = [Ks -Ls';-Ls zeros(3,3)];
        Mo = [Ms+Mfs zeros(2*(Jz+1),3);zeros(3,2*(Jz+1)) zeros(3,3)];
    case 'Without'
        disp('Calculation WITHOUT compatibility condition for the structure')
        Ls = zeros(2,2*(Jz+1));
        Ls(1,1) = 1;
        Ls(2,2) = 1;
        Ko = [Ks -Ls';-Ls zeros(2,2)];
        Mo = [Ms+Mfs zeros(2*(Jz+1),2);zeros(2,2*(Jz+1)) zeros(2,2)]; 
end% switch Compatibility

% Displaying matrices
figure(1)
spy(Mo,'k*')
FigureName = strcat('Mass matrix'); 
set(1,'Name',FigureName);
set(1,'NumberTitle','off');
set(1,'color',[1 1 1]);
xlabel('n_c \in [1,N_C]')
ylabel('n_l \in [1,N_L]')
title('{\bf M}');

figure(2)
spy(Ko,'k*')
FigureName = strcat('Stiffness matrix'); 
set(2,'Name',FigureName);
set(2,'NumberTitle','off');
set(2,'color',[1 1 1]);
xlabel('n_c \in [1,N_C]')
ylabel('n_l \in [1,N_L]')
title('{\bf K}');

figure(3)
spy(Mfs,'k*')
FigureName = strcat('Added mass matrix'); 
set(3,'Name',FigureName);
set(3,'NumberTitle','off');
set(3,'color',[1 1 1]);
xlabel('n_c \in [1,N_C]')
ylabel('n_l \in [1,N_L]')
title('{\bf M}_A');


% Modal analysis
%---------------

% Calulating eigenmodes and eigenfrequencies
[v,D] = eig(Ko,Mo);
f = 1/2/pi*sqrt(diag(D));
[fo,Io] = sort(f);
vo = v(:,Io);
K = 4;% Number of modes

% Displaying frequencies
disp('Eigenfrequencies [Hz]')
f = fo(1:K)

% Displaying mode shapes
for k = 1:1:K
    Uk = vo(:,k);
    for j = 1:Jz+1            
         U(j,1) = Uk(2*j-1,1);
    end % j = 2:Jz+1
    Pk = -inv(Kf)*Rfs*Uk(1:2*(Jz+1));
    for j = 1:Jr+1
        for i = 1:Jz+1
            P(i,j) = Pk(i + (j-1)*(Jz+1),1);
        end% i = 1:Jz+1
    end% i = 1:Jz+1
    figure(k+3);
    hndl = plot(sign(U(Jz+1))*U/max(abs(U))*R/2,z,'r');
	set(hndl,'LineWidth',3);
    hold on;
    plot(zeros(Jz+1,1),z,'r-');
    hndl = contour(r,z,sign(U(Jz+1))*P,30);
    hndl = plot(zeros(Jz+1,1),z,'b-');
    set(hndl,'LineWidth',3);
    hndl = plot(R*ones(Jz+1,1),z,'b-');
    set(hndl,'LineWidth',3);
    hndl = plot(r,zeros(Jr+1,1),'b-');
    set(hndl,'LineWidth',3);
	hndl = plot(r,L*ones(Jr+1,1),'b-');
    set(hndl,'LineWidth',3);
    hold off;
    axis([-0.5*R 1.1*R -0.1*L 1.1*L]);
    set(gca,'XColor',[1 1 1]);
    set(gca,'YColor',[1 1 1]);
    FigureName = strcat('Mode #',num2str(k)); 
    set(k+3,'Name',FigureName);
    set(k+3,'NumberTitle','off');
    set(k+3,'color',[1 1 1]);
    set(gca,'XColor',[1 1 1]);
    set(gca,'YColor',[1 1 1]);
    set(k+3,'Position',[300 100 500*R 500*L]);
end% k = 1:1:K