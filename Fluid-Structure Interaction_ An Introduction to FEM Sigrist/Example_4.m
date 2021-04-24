%==========================================================================
% Fluid-Structure Interaction - An Introduction to Finite Element Coupling
% (c) Jean-François SIGRIST, 2015
%--------------------------------------------------------------------------
% Example #4 - Vibro-Acoustic Response of a Vibrating Ring Immerseed in an
% Unbounded Acoustic Fluid
% (Chapter #5)
% Symmetric and non-symmetric coupled formulations with matrix scaling ;
% analytical and numerical calculation ; BGT-0 radiation condition ;
% Fourier mode representation
%==========================================================================


% Initialisation
%---------------
clear all;
close all;

% Material & geometrical properties
% Dimensionnal data
R = 0.5;
Ro = 1.5;
h = 0.05;
mu = 7800;
eta = 0.01;
E = 2.1e11*(1+j*eta);
rho = 1000;
c = 1500;
f = [5:5:5000];% Frequency range of interest
I = 25;% Number of finite elements
M = 15;% Number of Fourier modes 
qo = 0;% Angle of post-processing - theta = 0

% Reference values
c_o = 1500;
rho_o = 1000;
R_o = 1;

% Non-dimensionnal data 
f=f*R_o/c_o;
R=R/R_o;
Ro=Ro/R_o;
h=h/R_o;
E=E/rho_o/c_o^2;
mu=mu/rho_o;
rho=rho/rho_o;
c=c/c_o;


% Computation of the frequency response at each frequency step
%-------------------------------------------------------------
% Coupled formulation
Formulation = 'Non-symmmetric';
%Formulation = 'Symmetric';

% 1D mesh
r = R+[0:1:I]*(Ro-R)/I;

for k = 1:1:length(f) % Loop on frequency
    ura = 0;
    urn = 0;
    for m = 0:1:M% Loop on Fourier modes
        % Assembling matrices - fluid and structure
        % Structure matrices - analytical calculation
        mS = mu*h*[1 0; 0 1];
        kS = E*h/R^2*[1+m^4*h^2/12/R^2 m*(1+m^2*h^2/12/R^2); m*(1+m^2*h^2/12/R^2) m^2*(1+h^2/12/R^2)];
        % Fluid matrices : mass and stiffness
        mF = zeros(I+1,I+1);
        kF = zeros(I+1,I+1);
        for i=1:1:I% Loop on elements
            % Numerical integration procedure
            L = 3;% Three-points Gauss scheme
            Ri = (r(i+1)+r(i))/2;
            li = (r(i+1)-r(i))/2;
            sL=[-0.774596669241483 0.000000000000000 0.774596669241483];% Integration points
            wL=[0.555555555555556 0.888888888888889 0.555555555555556];% Integration weights
            mFi = zeros(2,2);
            kFio = zeros(2,2);
            kFim = zeros(2,2);
            for l=1:1:L% Loop on integration points
                sl = sL(l);
                wl = wL(l);
                mFi = mFi + li/4*(Ri+sl*li)*[(1-sl)^2 (1+sl)*(1-sl); (1+sl)*(1-sl) (1+sl)^2]*wl;
                kFio = kFio + (Ri+sl*li)/4/li*[1 -1;-1 1]*wl;
                kFim = kFim + li/4/(Ri+sl*li)*[(1-sl)^2 (1+sl)*(1-sl); (1+sl)*(1-sl) (1+sl)^2]*wl;
            end;% for l=1:1:L
            kFi = kFio + m^2*kFim;
            mFi = mFi/c^2;
            % Localisation matrices
            Lfi = zeros(2,I+1);
            Lfi(1,i) = 1;
            Lfi(2,i+1) = 1;
            mF = mF + Lfi'*mFi*Lfi;
            kF = kF + Lfi'*kFi*Lfi;
        end% for i=1:1:I
        % Fluid matrices : damping
        cF = zeros(I+1,I+1);
        cF(I+1,I+1) = 1/c;
        %Fluid-structure coupling matrix
        rFS = zeros(2,I+1);
        rFS(1,1) = 1;
        
        % Frequency response - analytical calculation
        wRc = 2*pi*f(1,k)*R/c;
        JmwRc = besselj(m,wRc);
        YmwRc = bessely(m,wRc);
        dJmwRc = (besselj(m-1,wRc)-besselj(m+1,wRc))/2;
        dYmwRc = (bessely(m-1,wRc)-bessely(m+1,wRc))/2;
        mum = -(JmwRc - j*YmwRc)/(dJmwRc - j*dYmwRc)/wRc;
        Mm = pi*R*mS;
        Km = pi*R*kS;
        Mm(1,1) = Mm(1,1) + rho*pi*R^2*mum;
        Fm = zeros(2,1);
        Fm(1,1) = 1/(1+(m==0));
        Xm = (inv(Km - 4*pi^2*f(1,k)^2*Mm))*Fm;
        urm = Xm(1,1);
        ura = ura + urm*cos(m*qo);
        
        % Frequency response - numerical calculation
        switch Formulation
            case 'Non-symmetric'
                Mm = [pi*R*mS zeros(2,I+1); -rho*pi*R*rFS' pi*mF];
                Km = [pi*R*kS pi*R*rFS; zeros(I+1,2) pi*kF];
                Cm = [zeros(2,2) zeros(2,I+1); zeros(I+1,2) pi*Ro*cF];
                Fm = zeros(I+3,1);
                Fm(1,1) = 1/(1+(m==0));
                Xm = (inv(Km +j*2*pi*f(1,k)*Cm - 4*pi^2*f(1,k)^2*Mm))*Fm;
                urn = urn + Xm(1,1)*cos(m*qo);
            case 'Symmetric'
                Mm = [pi*R*mS zeros(2,I+1) rho*pi*R*rFS; ...
                     zeros(I+1,2) zeros(I+1,I+1) pi*mF;...
                     rho*pi*R*rFS' pi*mF -rho*pi*kF];
                Km = [pi*R*kS zeros(2,I+1) zeros(2,I+1); ...
                      zeros(I+1,2) 1/rho*pi*mF zeros(I+1,I+1); ...
                      zeros(I+1,2) zeros(I+1,I+1) zeros(I+1,I+1)];
                Cm = [zeros(2,2) zeros(2,I+1) zeros(2,I+1);
                      zeros(I+1,2) zeros(I+1,I+1) zeros(I+1,I+1);...
                      zeros(I+1,2) zeros(I+1,I+1) pi*rho*Ro*cF];
                Fm = zeros(2+2*(I+1),1);
                Fm(1,1) = 1/(1+(m==0));
                Am = Km - 4*pi^2*f(1,k)^2*Mm+j*pi^3*f(1,k)^3*Cm;
                Xm = Am\Fm;
                urn = urn + Xm(1,1)*cos(m*qo);     
        end%switch Formulation
    end% for m = 1:1:M
    Ura(1,k) = ura;
    Urn(1,k) = urn;
end% for i = 1:1:length(f)

% Dimensional results
f=f*c_o/R_o;% Frequency
Ura=Ura/rho_o/c_o^2; % Displacement - analytical
Urn=Urn/rho_o/c_o^2;% Displacement - numerical

% Displaysing results
figure(1)
hndl = semilogy(f,abs(Ura),'co');
set(hndl,'LineWidth',1.5);
hold on;
hndl = semilogy(f,abs(Urn),'c-');
set(hndl,'LineWidth',1.5);
hold off;
set(1,'Color',[1 1 1]);
FigureName = strcat('Frequency response');
xlabel('\bf f [Hz]');
ylabel('\bf u_r(f) [m]');
set(1,'Name',FigureName);
set(1,'NumberTitle','off');
