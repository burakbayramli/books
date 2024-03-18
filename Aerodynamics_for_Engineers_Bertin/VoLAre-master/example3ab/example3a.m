%% VoLAre Example 3a: Half-span F-16
%
% Anthony Ricciardi
% August 2017 
% https://github.com/vtpasquale/VoLAre
%
% Modified from a publicly-available F-16 aeroelastic model (F16MA41) 
% published by ZONA Technology, Inc. [1]
%
% ZAERO Version 9.2 Applications Manual Vol. 1. Section 2.5 Case 5: F-16 
% Aircraft with External Stores (F16MA41). November 2016.

%% Clear workspace and set path
clear all; close all; clc
addpath('..\VoLAre_Toolbox\')

%% Select input file
filename = 'f16ma41_mod.inp';

%% Initialize model
FEM = VoLAre_init(filename);

% Optionally view model before calculating induced influences & velocities
% plotData(3,FEM)

%% Induced Influence Coefficients
[w, Vind14] = VoLAre_induced(FEM);

%% C_L sweep
[C0,Dim,FEM,iter,fun,alpha,delta] = VoLAre_trim(FEM,100,w,Vind14);

%% Plot model
close all
clear Options
% Options.collocation = 'YES';
% Options.bound_cent = 'YES';
% Options.boxNums = 'YES';
Options.n_vec = 'NO';
Options.CORD2R = 'YES';
Options.delta_pressures = 'YES';
% Options.Spanwise_Cl = 'YES';
% Options.forceVecs = Dim.F;

plotData(3,FEM,Options)

