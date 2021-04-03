function cfdReadOpenFoamFiles
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018 
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function opens OpenFoam files and reads required data (mesh, 
%   system files, transport properties ... etc)
%--------------------------------------------------------------------------
%
% Read and Setup OpenFOAM Mesh from "constant" Directory
%
cfdReadPolyMesh;
%
% Read fvSchemes, controlDict and fvSolution from "system" Directory
%
cfdReadSystem;
%
% Read Initial and Boundary Conditions from time Directory
%
cfdReadTimeDirectory;
%
% Read Transport Properties from "constant" Directory
%
cfdReadTransportProperties;
%
% Read Thermophysical Properties from "constant" Directory
%
cfdReadThermophysicalProperties;
%
% Read Turbulence Properties from "constant" Directory
%
cfdReadTurbulenceProperties;
%
% Read Gravitational Properties from "constant" Directory
%
cfdReadGravity;
