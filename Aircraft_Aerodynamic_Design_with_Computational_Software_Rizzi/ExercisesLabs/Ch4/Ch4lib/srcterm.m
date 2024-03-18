%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created Apr. 11 2005             %
%                   Last modified: May 10 2005                      %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: srcterm.m                                                   %
% Purpose: Calculates sourceterm and adds to rhs.                   %
% Called by: solveexplicit.m                                        %
% Calls:                                                            %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [handles] = srcterm(handles)
ib2 = handles.Data.Nodes -1;
a   = handles.Data.a;
p   = handles.Data.p;
rhs = handles.Data.rhs;

da = 0.5*(a(3:end)-a(1:(ib2-1)));
da = [da(1); da; da(end)];
rhs(:,2) = rhs(:,2)-p.*da;

handles.Data.rhs = rhs;