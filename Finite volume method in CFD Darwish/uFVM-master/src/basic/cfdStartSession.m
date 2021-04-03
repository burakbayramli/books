function cfdStartSession
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Post CFDLAB header and startup region
%--------------------------------------------------------------------------

% Necessary steps
clear all;
close all;
clc;

% Header
cfdPrintMainHeader;

% Setup data base
cfdSetupRegion;

% Print case directory
cfdPrintCaseDirectoyPath

