% JO jameson to add MG
clear all
close all
format compact
clc
%              #steps, work     #steps, work, #levels  factor
% # nodes      Jameson RK       Vcycle MG  
%     6         269,   1614
%    11         341,   3751
%    21         644,  13524
%    41        1300,  53300      400,  29200     3      1.82
%    81        2771, 224451      440,  67760     4      3.31                   
%   161        5059, 814499      536, 168304     5      4.84

Data.gamma   = 1.4;     % Gamma, air
Data.R       = 287.3;   % Gas Constant, air

Data.limfac  = 0.05;    % Limiter coefficient. (K in eq. 5.65)
Data.convtol = 1e-6;    % Solution accuracy.
Data.epsentr = 0.05;    % Entropy correction coefficient. (Delta in eq. 4.92)

% Do Not edit if uncertain of what you are doing
Data.nrk = 5;           % Number of Runge-Kutta stages (max 5).
Data.ark = [0.0533 0.1263 0.2375 0.4414 1]'; % Runge-Kutta stage coefficients.
%Data.ark = [0.0695 0.1602 0.2898 0.5060 1]';
% nodes   = [10   50   300 ];
% Ain     = [0.5  1.5   3   ];
% Aout    = [1    2.5   5   ];
% xShock  = [0.5  0.7   1   ];
% p01     = [30   100   200 ];
% t01     = [210  288   350 ];
% p2      = [30   70    200 ];
% iterimpl= [1    400   2000];
% iterexpl= [1    40    2000];
% vis2    = [0    0.25  8   ];
% vis4    = [0    0.02  0.5 ];
% CFLnum  = [0    1     4   ];

handles.Data = Data;
handles.Data.Levmax     = 4   % finest
levmax  = handles.Data.Levmax;
level = levmax
handles.Data.Nodes0     = 11; % number of nodes on coarsest
handles.Data.InletArea  = 1.5;
handles.Data.OutletArea = 2.5;
handles.Data.ShockPos   = 0.7;
handles.Data.p01 = 100*1e3;
handles.Data.t01 = 288;
handles.Data.p2  = 60*1e3;
handles.Data.TimestepsExplicit = 4000;
handles.Data.Vis2Explicit = 0.25;
handles.Data.Vis4Explicit = 0.04;
handles.Data.CFLNumber    = 0.5;
handles.Data.PlotInterval = 4;
handles.Data.Nstps        = 5;

handles.Data.Ncoarse      = 5;
handles.Data.Npre         = 3;
handles.Data.Npost        = 2;

handles = InitEmpty_MG(handles);
handles = handle_grid_MG(handles);
%handles = exact_solution(handles);
handles = iniflow_MG(handles);
handles = bcond_MG(handles);
handles = tstep_MG(handles);
handles.Data.iter = 0
doRK = 0
nxtit = 1
while handles.Data.iter <= handles.Data.TimestepsExplicit %&& handles.Data.drho >= handles.Data.convtol
    handles.Data.Wold = handles.Data.W;
    if doRK
        handles           = RK_MG(handles,handles.Data.Nstps,level);
        handles.Data.iter = handles.Data.iter + handles.Data.Nstps;
        handles = convergence_MG(handles);
    else
        handles           = MGVcycle(handles, zeros(handles.Data.Nodes{levmax},3), levmax);
        handles.Data.iter = handles.Data.iter + handles.Data.Npre+handles.Data.Npost;
        handles = convergence_MG(handles);
    end
    
    handles.Data.dthist = [handles.Data.dthist; handles.Data.dt(1)];
    handles.Data.iplot  = handles.Data.iplot + 1;
    if handles.Data.iter >= nxtit
        nxtit = nxtit + 30;
        figure(10)
        ww = handles.Data.p{levmax};
        clf
        plot(handles.Data.x{levmax},ww,'.k')
        title(num2str(handles.Data.iter))
        pause
    end
    if handles.Data.convhist(end) < handles.Data.convtol
        break
    end
    if ~doRK
      %  pause
    end
end
figure(handles.Data.Levmax + 1)
semilogy(handles.Data.convhist,'.-k')
title(['# stps ',num2str(handles.Data.iter)])
