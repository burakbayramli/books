%=========================================================================
% Fluid-Structure Interaction - An Introduction to Finite Element Coupling
% (c) Jean-François SIGRIST, 2015
%-------------------------------------------------------------------------
% Example #2 - Acoustic Modes of an Axi-Symmetric Cavity
% (Chapter #3)
% Imposed pressure p=0 at z=L with DOFs elimination
%=========================================================================

% Initialisation
%---------------
clear all;
close all;

% Material & geometrical properties
L = 1;
R1 = 0.1;
R2 = 0.2;
c = 1500;
m = 1;% Fourier mode


% Matrices
%---------

% 2D mesh
Jr = 20;
for j = 1:1:Jr+1
    r(j,1) = R1+(j-1)*(R2-R1)/Jr;
end% j = 1:1:Jr+1
Jz = 40;
for j = 1:1:Jz+1
    z(j,1) = (j-1)*L/Jz;
end% j = 1:1:Jz+1

% Assembling mass and stiffness fluid matrices
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
      mfij = (aj*bi/4/(c^2))*(r0+(r0/3)*e*e'+(aj/3)*(e*Id'+Id*e')).*(1+(1/3)*n*n');
      % Elementary stiffness matrix
      kf1  = aj*bi*r0*(((e*e')/4/(aj^2)).*(1+(n*n')/3)+(n*n'/4/(bi^2)).*(1+(e*e')/3)) + (aj^2/12/bi)*(n*n').*(e*Id'+Id*e');
      kf2  = bi/8*(2*(e*Id'+Id*e')-2*(r0/aj)*e*e'+(1-(r0/aj)*(e*Id'+Id*e')+((r0/aj)^2)*e*e')*log((r0+aj)/(r0-aj))).*(1+(n*n')/3);
      kfij = kf1 + (m^2)*kf2;
      % Localisation matrices
      Lij=zeros(4,(Jz+1)*(Jr+1));
      Lij(1,i   + (j-1)*(Jz+1)) = 1;
      Lij(2,i   + j*(Jz+1))     = 1;
      Lij(3,i+1 + j*(Jz+1))     = 1;
      Lij(4,i+1 + (j-1)*(Jz+1)) = 1;  
      % Mass and stiffness matrices
      Kf = Kf + Lij'*kfij*Lij;
      Mf = Mf + Lij'*mfij*Lij;
   end% for i = 1:1:Jz
end% for j = 1:Jr

% Eliminating constrained DOFs
q=1;
for j=1:1:Jr+1
    for i = 1:1:Jz
        J(q)=i+(j-1)*(Jz+1);
        q=q+1;
    end% j=1:1:Jr+1
end% i=1:1:Jz+1
Ko = Kf(J,J);
Mo = Mf(J,J);

% Displaying matrices
figure(1)
spy(Mo,'k*')
FigureName = strcat('Mass matrix'); 
set(1,'Name',FigureName);
set(1,'NumberTitle','off');
set(1,'color',[1 1 1]);
xlabel('n_c \in [1,N_C]')
ylabel('n_l \in [1,N_L]')
title('{\bf M}_F');

figure(2)
spy(Ko,'k*')
FigureName = strcat('Stiffness matrix'); 
set(2,'Name',FigureName);
set(2,'NumberTitle','off');
set(2,'color',[1 1 1]);
xlabel('n_c \in [1,N_C]')
ylabel('n_l \in [1,N_L]')
title('{\bf K}_F');


% Modal analysis
%---------------

% Calulating eigenmodes and eigenfrequencies
[v,D] = eig(Ko,Mo);
f = 1/2/pi*sqrt(diag(D));
f = sort(f);
D = diag(D);
D = real(D);
K = 4;% Number of modes

% Displaying frequencies
disp('Eigenfrequencies [Hz]')
f = f(1:K)

% Displaying mode shapes
for k = 1:1:K
    [minD,Ik] = min(D);
    D(Ik) = max(D);
    Pk = zeros(Jz*(Jr+1),1);
    Pk = v(1:Jz*(Jr+1),Ik);
    P = zeros(Jz+1,Jr+1);  
    for j = 1:Jr+1
        for i = 1:Jz
             P(i,j) = Pk(i + (j-1)*Jz,1);
        end% i = 1:Jz+1
    end% j = 1:Jr+1
    P = real(P);
    figure(k+2);
    contour(r,z,P,50);
    hold on;
    hndl = plot(R2*ones(Jz+1,1),z,'b-');
    set(hndl,'LineWidth',5);
    hndl = plot(R1*ones(Jz+1,1),z,'b-');
    set(hndl,'LineWidth',5);
    hndl = plot(r,zeros(Jr+1,1),'b-');
    set(hndl,'LineWidth',5);
    hndl = plot(r,L*ones(Jr+1,1),'b-');
    set(hndl,'LineWidth',5);  
    hold off;
    FigureName = strcat('Mode #',num2str(k)); 
    set(k+2,'Name',FigureName);
    set(k+2,'NumberTitle','off');
    zlabel('P(r,z) [bar]');
    set(k+2,'color',[1 1 1]);
    axis([R1 R2 0 L]);
    set(gca,'XColor',[1 1 1]);
    set(gca,'YColor',[1 1 1]);
    set(k+2,'Position',[300 100 300 500]);
end% k = 1:1:K

