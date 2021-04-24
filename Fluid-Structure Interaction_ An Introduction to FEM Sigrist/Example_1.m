%=========================================================================
% Fluid-Structure Interaction - An Introduction to Finite Element Coupling
% (c) Jean-François SIGRIST, 2015
%-------------------------------------------------------------------------
% Example #1 - Bending Modes of a Straight Elastic Beam
% (Chapter #2)
% Calculation with Lagrange formulation
%=========================================================================

% Initialisation
%---------------
clear all;
close all;

% Material & geometrical properties
L = 1;
mu = 7800;
R = 0.1;
h = 0.01;
E = 2.1e11;
S = pi*(R^2-(R-h)^2);% Cross-section area
I = pi*(R^4-(R-h)^4)/4;% Cross-section inertia


% Matrices
%---------

% 1D mesh
Jz = 40;
for j = 1:1:Jz+1
    z(j,1) = (j-1)*L/Jz;
end%j = 1:1:J+1

% Assembling mass and stiffness structure matrices
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

% Assembling matrices
Ls = zeros(2,2*(Jz+1));
Ls(1,1) = 1;
Ls(2,2) = 1;
Ko = [Ks -Ls';-Ls zeros(2,2)];
Mo = [Ms zeros(2*(Jz+1),2);zeros(2,2*(Jz+1)) zeros(2,2)];

% Displaying matrices
figure(1)
spy(Mo,'k*')
FigureName = strcat('Mass matrix'); 
set(1,'Name',FigureName);
set(1,'NumberTitle','off');
set(1,'color',[1 1 1]);
xlabel('n_c \in [1,N_C]')
ylabel('n_l \in [1,N_L]')
title('{\bf M}_S');

figure(2)
spy(Ko,'k*')
FigureName = strcat('Stiffness matrix'); 
set(2,'Name',FigureName);
set(2,'NumberTitle','off');
set(2,'color',[1 1 1]);
xlabel('n_c \in [1,N_C]')
ylabel('n_l \in [1,N_L]')
title('{\bf K}_S');


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
    Uk = zeros(2*(Jz+1),1);
    Uk = vo(:,k);
    U = zeros (Jz+1,1);
    for j = 1:Jz+1            
         U(j,1) = Uk(2*j-1,1);
    end % j = 2:Jz+1
    % Calculation of shear force and moment
    Tk = zeros(Jz+1,1);
    Mk = zeros(Jz+1,1);
    for j=1:1:Jz
        lj = z(j+1,1)-z(j,1);
        ddNi = E*I*[-6/lj^2 -4/lj 6/lj^2 -2/lj];
        dddNi = E*I*[12/lj^3 6/lj^2 -12/lj^3 6/lj^2];
        Mk(j,1) = ddNi*[Uk(2*(j-1)+1); Uk(2*(j-1)+2); Uk(2*j+1); Uk(2*j+2)];
        Tk(j,1) = -dddNi*[Uk(2*(j-1)+1); Uk(2*(j-1)+2); Uk(2*j+1); Uk(2*j+2)];
    end% for j=1:1:Jz
    disp(strcat('Mode #',num2str(k),' :'))
    disp('Shear force at z = 0')
    disp('- from Lagrange multiplier')
    R_o = vo(2*(Jz+1)+1,k)/U(Jz+1)/E/I/L^3
    disp('- from Finite Element shape function')
    T_0 = Tk(1)/U(Jz+1)/E/I/L^3
    disp('Shear moment at z =0')
    disp('- from Lagrange multiplier')
    M_o = vo(2*(Jz+1)+2,k)/U(Jz+1)/E/I/L^2
    disp('- from Finite Element shape function')
    M_0 = Mk(1)/U(Jz+1)/E/I/L^2
    figure(k+2); 
    hndl = plot(sign(U(Jz+1))*U/max(abs(U)),z,'r');
    set(hndl,'LineWidth',2.5);
    hold on;
    plot(zeros(Jz+1,1),z,'r-');
    hndl = compass(sign(R_o)/2,0,'m');
    set(hndl,'LineWidth',2.5);
    hndl = quiver(zeros(Jz+1,1),z,Tk*sign(U(Jz+1)),zeros(Jz+1,1),'m');
    set(hndl,'LineWidth',1.25);
    hold off;
    axis([-1 1 -0.05 L]);
    FigureName = strcat('Mode #',num2str(k)); 
    set(k+2,'Name',FigureName);
    set(k+2,'NumberTitle','off');
    set(k+2,'color',[1 1 1]);
    set(gca,'XColor',[1 1 1]);
    set(gca,'YColor',[1 1 1]);
    set(k+2,'Position',[300 100 300 500]);
end% k = 1:1:K