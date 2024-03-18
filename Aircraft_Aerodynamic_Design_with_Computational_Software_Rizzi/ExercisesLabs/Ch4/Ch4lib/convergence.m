%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created May 10  2005             %
%                   Last modified: May 10 2005                      %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: convergence.m                                               %
% Purpose: Monitor and store convergence.                           %
% Called by: solveexplicit.m                                        %
% Calls:                                                            %
%                                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function handles = convergence(handles)

nnodes = handles.Data.Nodes;
W      = handles.Data.W;
Wold   = handles.Data.Wold;
%iter   = handles.Data.iter;
convhist = handles.Data.convhist;
drhoold  = handles.Data.drhoold;

% Calculate (2-norm) density difference.
% RMS A rho
drho = sqrt((sum((W(:,1) - Wold(:,1)).^2))/(nnodes+1)); % Eq. 12.1 in Blazek.

% Normalize with first density difference.
if isempty(convhist)
    drhoold = drho;
    handles.Data.drhoold  = drhoold;
end
drho = drho/drhoold;
%disp([' dhro: ',num2str(drho)]);
% Store convergence history.
convhist = [convhist drho];
handles.Data.drho     = drho;
handles.Data.convhist = convhist;