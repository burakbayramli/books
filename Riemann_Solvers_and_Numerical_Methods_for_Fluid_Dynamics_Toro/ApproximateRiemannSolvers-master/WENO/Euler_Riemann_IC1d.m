function [r0,u0,p0,tEnd,cfl] = Euler_Riemann_IC1d(x,input)
% Load the IC of a classical 1D Riemann Problems.
%
% Coded by Manuel A. Diaz, NTU, 2013.1.24.
%
% The proporties of the problem are arranged as:
%
%       prop = [prop_left , prop_right]
% 
% Notation:
% u   = Velocity in x direction
% p   = Pressure
% rho = Density
% r   = Fugacity
% E   = Enerty
% t   = temperature
%
%% Riemann Problems
switch input
    case{1} % Configuration 1, Sod's Problem
        fprintf('Case 1: Sods problem \n');
        p   = [1    0.1  ];
        u   = [0    0    ];
        rho = [1    0.125];
        tEnd = 0.1; cfl = 0.90;
        
    case{2} % Configuration 2, Left Expansion and right strong shock
        fprintf('Case 2: Left Expansion and right strong shock \n');
        p   = [1000 0.1  ];
        u   = [0    0    ];
        rho = [3    2    ];
        tEnd = 0.02; cfl = 0.90;
        
    case{3} % Configuration 3, Right Expansion and left strong shock
        fprintf('Case 3: Right Expansion and left strong shock \n');
        p   = [7    10   ];
        u   = [0    0    ];
        rho = [1    1    ];
        tEnd = 0.1; cfl = 0.90;
        
    case{4} % Configuration 4, Double Shock
        fprintf('Case 4: Double Shock \n');
        p   = [450  45   ];
        u   = [20   -6   ];
        rho = [6    6    ];
        tEnd = 0.02; cfl = 0.90;
        
    case{5} % Configuration 5, Double Expansion
        fprintf('Case 5: Double Expansion \n');
        p   = [40   40   ];
        u   = [-2   2    ];
        rho = [1    2.5  ];
        tEnd = 0.03; cfl = 0.90;

    case{6} % Configuration 6, Cavitation
        fprintf('Case 6: Cavitation \n');
        p   = [0.4  0.4  ];
        u   = [-2    2   ];
        rho = [ 1    1   ];
        tEnd = 0.1; cfl = 0.90;
        
    case{7} % Shocktube problem of G.A. Sod, JCP 27:1, 1978 
        fprintf('Shocktube problem of G.A. Sod, JCP 27:1, 1978');
        p   = [1.0  0.1  ];
        u   = [0.75 0    ];
        rho = [1    0.125];
        tEnd = 0.17; cfl = 0.90;
        
    case{8} % Lax test case: M. Arora and P.L. Roe: JCP 132:3-11, 1997
        fprintf('Lax test case: M. Arora and P.L. Roe: JCP 132:3-11, 1997');
        p   = [3.528 0.571];
        u   = [0.698 0    ];
        rho = [0.445 0.5  ];
        tEnd = 0.15; cfl = 0.90; 
      
    case{9} % Mach = 3 test case: M. Arora and P.L. Roe: JCP 132:3-11, 1997
        fprintf('Mach = 3 test case: M. Arora and P.L. Roe: JCP 132:3-11, 1997');
        p   = [10.333  1  ];
        u   = [ 0.92  3.55];
        rho = [ 3.857  1  ];
        tEnd = 0.09; cfl = 0.90;
        
    case{10} % Shocktube problem with supersonic zone
        fprintf('Shocktube problem with supersonic zone');
        p   = [1  0.02];
        u   = [0  0.00];
        rho = [1  0.02];
        tEnd = 0.162; cfl = 0.90; 
        
    otherwise 
        error('Case not available');
        
end
% Print for Riemann Problems
fprintf('\n');
fprintf('density (L): %1.3f\n',rho(1));
fprintf('velocity(L): %1.3f\n',u(1));
fprintf('Presure (L): %1.3f\n',p(1));
fprintf('\n');
fprintf('density (R): %1.3f\n',rho(2));
fprintf('velocity(R): %1.3f\n',u(2));
fprintf('Presure (R): %1.3f\n',p(2));
fprintf('\n');

%% Load Selected case Initial condition:
% Pre-Allocate variables
r0 = zeros(size(x)); 
u0 = zeros(size(x)); 
p0 = zeros(size(x));

% Parameters of regions dimensions
x_middle = (x(end)-x(1))/2;
L = find(x<x_middle);
R = find(x>=x_middle);

% Initial Condition for our 2D domain
% Density
r0(L) = rho(1); % region 1
r0(R) = rho(2); % region 2
% Velovity in x
u0(L) = u(1); % region 1
u0(R) = u(2); % region 2
% temperature
p0(L) = p(1); % region 1
p0(R) = p(2); % region 2