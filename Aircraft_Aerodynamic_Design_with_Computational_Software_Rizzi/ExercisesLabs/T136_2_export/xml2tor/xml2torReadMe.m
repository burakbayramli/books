% Readme for xml2tornadofun package
%==================================
% Javier, MengMeng
% Javier: to plot CEASIOM aircraft struct
%
% MengMeng: [xcent,zcent,rzcent,rycent]=fusev(fuse);
% computes the contors (xz and xy) of the fuselage. fuse is
% the ac.Fuselage
% You may want to try computing a case with new control surfaces
% look in rxml2to lline 34 and set run = 1.
% try also an older tornado library; 
%==================================
% 091022
% directory:
% .
%    mfile, xmlfiles, ...
% ./T135-003_EXPORT (the most recent one)
%                  mfiles,...
%                  /aircraft
%                  /airfoil
% ./XMLtoolbox
%                 pfiles,...
% Main routine xml2tornadofun
% takes a CEASIOM aircraft struct and makes the tornado struct
% and plots (if desired) a wireframe representation including
% airfoil profiles.
% To test:
% run the rxml2to ... after possible changing to correct paths for xml toolbox
% and tornado T135_003EXPORT (the new one is provided)
% choose a CEASIOM xml geo file ( B747 (with inneraileron), B747
% (standard), ... provided
% a wireframe plot with airfoil profiles and coloured
% control surfaces (undeflected!) should appear infigure 15 