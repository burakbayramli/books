
addpath Codes1D
addpath Codes2D
addpath Codes3D
addpath ServiceRoutines
addpath CFD1D
addpath CFD2D
addpath Grid/
addpath Grid/CFD
addpath Grid/3D
addpath Grid/CNS2D
addpath Grid/Euler2D
addpath Grid/Maxwell2D
addpath Grid/Other

cpt = cputime;

EulerDriver1D   
figure; plot(x,Ener); drawnow; pause(.1);                

PoissonCDriver1D
figure; plot(x,u); drawnow; pause(.1);                

AdvecDriver1D                 
figure; plot(x,u); drawnow; pause(.1);                

BurgersDriver1D               
figure; plot(x,u); drawnow; pause(.1);                

HeatDriver1D                  
figure; plot(x,u); drawnow; pause(.1);                

MaxwellDriver1D               
figure; plot(x,E); drawnow; pause(.1);                


CurvedCNSDriver2D               
figure; PlotField2D(N, x, y, Q(:,:,4)); drawnow; pause(.1);                

CurvedPoissonIPDGDriver2D
figure; PlotField2D(N, x, y, u); drawnow; pause(.1);                

CurvedEulerDriver2D             
figure; PlotField2D(N, x, y, Q(:,:,4)); drawnow; pause(.1);                

MaxwellCurvedDriver2D
figure; PlotField2D(N, x, y, Ez); drawnow; pause(.1);                

CurvedINSDriver2D               
figure; PlotField2D(N, x, y, PR); drawnow; pause(.1);                

MaxwellDriver2D
figure; PlotField2D(N, x, y, Ez); drawnow; pause(.1);                

EulerDriver2D                   
figure; PlotField2D(N, x, y, Q(:,:,4)); drawnow; pause(.1);                

MaxwellHNonConDriver2D
figure; PlotField2D(N, x, y, Ez); drawnow; pause(.1);                

EulerShockDriver2D              
figure; PlotField2D(N, x, y, Q(:,:,4)); drawnow; pause(.1);                

PoissonDriver2D
figure; PlotField2D(N, x, y, u); drawnow; pause(.1);                

MaxwellPNonConDriver2D

AdvecDriver3D
figure; PlotContour3D(N, u, linspace(0, 1, 5)); drawnow; pause(.1);                

MaxwellDriver3D
figure; PlotContour3D(3*N, Ez, linspace(-1, 1, 10)); drawnow; pause(.1);                

PoissonIPDGDriver3D
figure; PlotContour3D(3*N, u, linspace(.1, 1, 10)); drawnow; pause(.1);                
hold on
PlotSlice3D(2*N, u, 'x', .4);
hold off

alldemostook = cputime-cpt


