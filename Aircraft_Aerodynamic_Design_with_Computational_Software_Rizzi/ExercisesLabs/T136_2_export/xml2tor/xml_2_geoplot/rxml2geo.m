% rxml2geo
% run the xml2to
% needs:
% xml toolbox: xml_load
% xml2geo
% 
clear all
close all
format compact
home        = pwd;
datahomedir = home;
path(path,'C:\Documents and Settings\jespero\My Documents\simsac\CEASIOM 75b\XMLToolbox');

% [home,filesep,'T135-003_EXPORT'];

% airfoil library
[fname,pathname,filtind] = uigetfile('*.xml','choose xml geo file');
dd                       = dir([pathname,filesep,fname]);

ac   = xml_load([pathname,filesep,fname]);
doplot = 1;
XYZ = xml2geo(ac,doplot);