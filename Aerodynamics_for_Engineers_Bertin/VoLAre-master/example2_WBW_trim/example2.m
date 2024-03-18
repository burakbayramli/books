%% VoLAre Example 2: Swept wing with tail and elevator trim
%  This example is used to check two-variable trim solve
%
% Anthony Ricciardi
% August 2017 
% https://github.com/vtpasquale/VoLAre
%
% Verification case produced using AVL 3.27 [1]
% [1] Drela, M. and Youngren, H., ``AVL User Primer'', Accessed 2013.

%% Clear workspace and set path
clear all; close all; clc
addpath('..\VoLAre_Toolbox\')

%% Select input file
filename = 'wingWithTail.inp';

%% Initialize model
FEM = VoLAre_init(filename);

% Optionally view model before calculating induced influences & velocities
% plotData(3,FEM)

%% Calculate induced influences & velocities
% This is the most computational expensive step.  It is separated from the
% trim solve function so the output can be reused for multiple trim solves
[w, Vind14] = VoLAre_induced(FEM);

%% C_L sweep
TRIM = 100:100:400;
for i = 1:4
    [C(i),Dim(i),FEM_Out(i),iter(i),fun(:,i),alpha(i),delta(i)] = VoLAre_trim(FEM,TRIM(i),w,Vind14);
end

%% Results Comparison
avl = readtable('AVL_trim_results.csv','HeaderLines',1);

%% Plot lift and drag curves
figure(1)
plot([0,[C.L]],[0,alpha],'o-',...
     [0,[C.L]],[0,delta],'o-',...
     avl.C_L,avl.alpha,'o-',...
     avl.C_L,avl.delta,'o-')
title('Trim Angles')
xlabel('C_L'); ylabel('Angle (deg.)'); 
legend('VoLAre \alpha','VoLAre \delta','AVL \alpha','AVL \delta','location','east');

figure(2)
plot([0,[C.L]],[0,[C.D]],'o-',...
     avl.C_L,avl.C_D,'o-')
title('Trimmed Drag Polar')
xlabel('C_L'); ylabel('C_D'); 

legend('VoLAre','AVL','location','northwest');

%% Plot model
clear Options
% Options.collocation = 'YES';
% Options.bound_cent = 'YES';
% Options.boxNums = 'YES';
Options.n_vec = 'NO';
% Options.CORD2R = 'YES';
Options.delta_pressures = 'YES';
% Options.Spanwise_Cl = 'YES';
% Options.forceVecs = F;

plotData(3,FEM_Out(1),Options)