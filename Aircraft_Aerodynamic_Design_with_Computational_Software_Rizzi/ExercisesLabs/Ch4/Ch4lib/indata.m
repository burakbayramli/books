function [RangeVar, PlotParam, Data] = indata();

% The sliders for each variable are defined accordingly to:
% variabel = [min, value, max]

nodes   = [10   50   300 ];
Ain     = [0.5  1.5   3   ];
Aout    = [1    2.5   5   ];
xShock  = [0.5  0.7   1   ];
p01     = [30   100   200 ];
t01     = [210  288   350 ];
p2      = [30   70    200 ];
iterimpl= [1    400   2000];
iterexpl= [1    40    2000];
vis2    = [0    0.25  8   ];
vis4    = [0    0.02  0.5 ];
CFLnum  = [0    1     4   ];

RangeVar = [nodes; Ain; Aout; ...
            xShock; p01; t01; ...
            p2; iterimpl; iterexpl; ...
            vis2; vis4; CFLnum;];  
        
PlotParam = [1, 10, 50]; % Parameters controling the range of the plot slider 

Data.gamma = 1.4;       % Gamma
Data.R = 287.3;         % Gas Constant

Data.limfac  = 1e-12;   % Limiter coefficient. (K in eq. 5.65)
Data.convtol = 1e-6;    % Solution accuracy.
Data.epsentr = 0.005; % Entropy correction coefficient. (Delta in eq. 4.92)

% Do Not edit if uncertain of what you are doing

Data.ark = [0.0533 0.1263 0.2375 0.4414 1]'; % Runge-Kutta stage coefficients.         
Data.ark = [0.0695 0.1602 0.2898 0.5060 1];
%Data.ark = [1/2 1];
%Data.ark = [2/3 1/2 1];
%Data.ark = [1];
Data.nrk = length(Data.ark);  % Number of Runge-Kutta stages (max 5).
Data.ntim    = 4;       % Magimum numbers of retakes in bwe1
Data.maxiter = 15;      % Maximium numbers if iterations in bwe1
Data.tol     = 1e-5;    % Maximum condition number on the jacobian in bwe1