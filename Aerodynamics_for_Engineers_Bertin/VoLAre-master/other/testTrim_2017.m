clear all; close all; clc
addpath('..\VoLAre_Toolbox\')

%%
filename = 'fineMesh.bdf';

FEM = importFieldModelAdvanced(filename);
FEM = createPanelModel(FEM);

%% free stream
% Airspeed = 25;
% rho = .0024;
% QINF = .5*rho*Airspeed^2;

%% Induced Influence Coefficients
[w, Vind14] = VoLAre_induced(FEM);


%% C_L sweep
TRIM = 100:100:500;
for i = 1:5
    [C(i),Dim(i),FEM_Out(i),iter(i),fun(i),alpha(i)] = VoLAre_trim_2017(FEM,TRIM(i),w,Vind14);
end

%% Plot lift and drag curves
plot(alpha,[C.L],'o-')

%% Plot model

close all
clear Options
% Options.collocation = 'YES';
% Options.bound_cent = 'YES';
% Options.boxNums = 'YES';
Options.n_vec = 'YES';
% Options.CORD2R = 'YES';
% Options.delta_pressures = 'YES';
Options.Spanwise_Cl = 'YES';
% Options.forceVecs = F;

% plotData(1,FEM,Options)

