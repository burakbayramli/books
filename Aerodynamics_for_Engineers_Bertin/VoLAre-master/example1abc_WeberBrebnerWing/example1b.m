%% VoLAre Example 1b: Half-span swept wing analysis using fine mesh.
%
% Anthony Ricciardi
% August 2017 
% https://github.com/vtpasquale/VoLAre
%
% o Planform described and tested by Weber and Brebner [1]
% o Verification case produced using AVL 3.27 [2]
% 
% [1] Weber, J. and Brebner, G. G. ``Low-speed Tests on 45-deg Sweptback 
%     Wings, Part I: Pressure Measurements on Wings of Aspect Ratio 5. In 
%     Reports and Memoranda 2882, Aeronautical Research Council,'' 1958.
% [2] Drela, M. and Youngren, H., ``AVL User Primer'', Accessed 2013.

%% Clear workspace and set path
clear all; close all; clc
addpath('..\VoLAre_Toolbox\')

%% Select input file
filename = 'fineMesh.inp';

%% Initialize model
FEM = VoLAre_init(filename);

% Optionally view model before calculating induced influences & velocities
% plotData(3,FEM)

%% Calculate induced influences & velocities
% This is the most computational expensive step.  It is separated from the
% trim solve function so the output can be reused for multiple trim solves
[w, Vind14] = VoLAre_induced(FEM);

%% C_L sweep
% A conventional angle of attack (AoA) sweep is not describable by the 
% adopted ZAERO input formatting.  An analogous result is obtained through 
% a C_L sweep, which is executed using a series of trim solves where AoA is
% the only trim variable.  
TRIM = 100:100:500;
for i = 1:5
    [C(i),Dim(i),FEM_Out(i),iter(i),fun(i),alpha(i)] = VoLAre_trim(FEM,TRIM(i),w,Vind14);
end

%% Results Comparison
wb = readtable('Weber_and_Brebner.csv','HeaderLines',1);
avl = readtable('AVL_fine_mesh.csv','HeaderLines',1);
legendStr = {'VoLAre','AVL 3.27','Wind Tunnel'};

%% Plot lift and drag curves for verification and validation
set(0,'defaulttextfontsize',14);
set(0,'defaultlinelinewidth',1);

figure(1)
plot([0,alpha],[0,[C.L]],'o-',avl.AoA_deg,avl.C_L,'o-',wb.AoA_deg,wb.C_L,'o-')
title('C_L Comparison')
ylabel('C_L'); xlabel('\alpha (deg.)'); legend(legendStr,'location','southeast');

figure(2)
plot([0,alpha],[0,[C.D]],'o-',avl.AoA_deg,avl.C_D,'o-',wb.AoA_deg,wb.C_D,'o-')
title('C_D Comparison')
ylabel('C_D'); xlabel('\alpha (deg.)'); legend(legendStr,'location','northwest');

%% Plot model
% Select plot options -> defaults = NO for all

clear Options
% Options.collocation = 'YES';
% Options.bound_cent = 'YES';
% Options.boxNums = 'YES';
Options.n_vec = 'NO';
% Options.CORD2R = 'YES';
Options.delta_pressures = 'YES';
% Options.Spanwise_Cl = 'YES';
% Options.forceVecs = F;

figure(3);
plotData(3,FEM_Out(1),Options)
axis equal