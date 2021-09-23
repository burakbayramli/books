function [r_0,u_0,v_0,p_0] = Euler_IC2d(x,y,input)
% Load the IC of a 1D Riemann classical schok tube problem configuration. 
% In the notation we take advantage of the matlab array notation as follows
%
%   1.0 +-----------+-----------+
%       |           |           |       
%       |   reg 2   |   reg 1   |
%       |           |           |
%   0.5 +-----------+-----------+
%       |           |           |
%       |   reg 3   |   reg 4   |
%       |           |           |
%   0.0 +-----------+-----------+
%      0.0         0.5         1.0
%
% prop = [prop_reg1 , prop_reg2 , prop_reg3 , prop_reg4]
%
%   r = rho/density
%   u = velocity in x direction
%   v = velocity in y direction
%   p = Pressure
%
% Manuel Diaz, NTU, 2014.06.27

%% Initial Physical Properties per case:
switch input
    case{1} % Configuration 1
        fprintf('Configuration 1 \n');
        p = [1.0  0.4     0.0439  0.15  ];
        r = [1.0  0.5197  0.1072  0.2579];
        u = [0.0 -0.7259 -0.7259  0.0   ];
        v = [0.0 -0.0    -1.4045 -1.4045];
        
    case{2} % Configuration 2
        fprintf('Configuration 2 \n');
        p = [1.0  0.4     1.0     0.4   ];
        r = [1.0  0.5197  1.0     0.5197];
        u = [0.0 -0.7259 -0.7259  0.0   ];
        v = [0.0  0.0    -0.7259 -0.7259];
        
    case{3} % Configuration 3
        fprintf('Configuration 3 \n');
        p = [1.5 0.3    0.029 0.3   ];
        r = [1.5 0.5323 0.138 0.5323];
        u = [0.0 1.206  1.206 0.0   ];
        v = [0.0 0.0    1.206 1.206 ];
        
    case{4} % Configuration 4
        fprintf('Configuration 4 \n');
        p = [1.1 0.35   1.1    0.35  ];
        r = [1.1 0.5065 1.1    0.5065];
        u = [0.0 0.8939 0.8939 0.0   ];
        v = [0.0 0.0    0.8939 0.8939];
        
    case{5} % Configuration 5
        fprintf('Configuration 5 \n');
        p = [ 1.0   1.0   1.0   1.0 ];
        r = [ 1.0   2.0   1.0   3.0 ];
        u = [-0.75 -0.75  0.75  0.75];
        v = [-0.5   0.5   0.5  -0.5 ];
        
    case{6} % Configuration 6
        fprintf('Configuration 6 \n');
        p = [ 1.0  1.0   1.0   1.0 ];
        r = [ 1.0  2.0   1.0   3.0 ];
        u = [ 0.75 0.75 -0.75 -0.75];
        v = [-0.5  0.5   0.5  -0.5 ];
        
    case{7} % Configuration 7
        fprintf('Configuration 7 \n');
        p = [1.0  0.4    0.4  0.4   ];
        r = [1.0  0.5197 0.8  0.5197];
        u = [0.1 -0.6259 0.1  0.1   ];
        v = [0.1  0.1    0.1 -0.6259];

    case{8} % Configuration 8
        fprintf('Configuration 8 \n');
        p = [0.4     1.0    1.0  1.0   ];
        r = [0.5197  1.0    0.8  1.0   ];
        u = [0.1    -0.6259 0.1  0.1   ];
        v = [0.1     0.1    0.1 -0.6259];
        
    case{9} % Configuration 9
        fprintf('Configuration 9 \n');
        p = [1.0  1.0  0.4     0.4   ];
        r = [1.0  2.0  1.039   0.5197];
        u = [0.0  0.0  0.0     0.0   ];
        v = [0.3 -0.3 -0.8133 -0.4259];
        
    case{10} % Configuration 10
        fprintf('Configuration 10 \n');
        p = [1.0    1.0     0.3333  0.3333];
        r = [1.0    0.5     0.2281  0.4562];
        u = [0.0    0.0     0.0     0.0   ];
        v = [0.4297 0.6076 -0.6076 -0.4297];
        
    case{11} % Configuration 11
        fprintf('Configuration 11 \n');
        p = [1.0 0.4    0.4 0.4   ];
        r = [1.0 0.5313 0.8 0.5313];
        u = [0.1 0.8276 0.1 0.1   ];
        v = [0.0 0.0    0.0 0.7276];
        
    case{12} % Configuration 12
        fprintf('Configuration 12 \n');
        p = [0.4    1.0    1.0 1.0  ];
        r = [0.5313 1.0    0.8 1.0  ];
        u = [0.0    0.7276 0.0 0.0  ];
        v = [0.0    0.0    0.0 0.7276];
        
    case{13} % Configuration 13
        fprintf('Configuration 13 \n');
        p = [ 1.0 1.0 0.4    0.4   ];
        r = [ 1.0 2.0 1.0625 0.5313];
        u = [ 0.0 0.0 0.0    0.0   ];
        v = [-0.3 0.3 0.8145 0.4276];
        
    case{14} % Configuration 14
        fprintf('Configuration 14 \n');
        p = [ 8.0     8.0    2.6667 2.6667];
        r = [ 2.0     1.0    0.4736 0.9474];
        u = [ 0.0     0.0    0.0    0.0   ];
        v = [-0.5606 -1.2172 1.2172 1.1606];
        
    case{15} % Configuration 15
        fprintf('Configuration 15 \n');
        p = [ 1.0  0.4     0.4 0.4   ];
        r = [ 1.0  0.5197  0.8 0.5313];
        u = [ 0.1 -0.6259  0.1 0.1   ];
        v = [-0.3 -0.3    -0.3 0.4276];
        
    case{16} % Configuration 16
        fprintf('Configuration 16 \n');
        p = [0.4     1.0    1.0 1.0  ];
        r = [0.5313  1.0222 0.8 1.0  ];
        u = [0.1    -0.6179 0.1 0.1  ];
        v = [0.1     0.1    0.1 0.8276];
        
    case{17} % Configuration 17
        fprintf('Configuration 17 \n');
        p = [ 1.0  1.0 0.4     0.4   ];
        r = [ 1.0  2.0 1.0625  0.5197];
        u = [ 0.0  0.0 0.0     0.0   ];
        v = [-0.4 -0.3 0.2145 -1.1259];
        
    case{18} % Configuration 18
        fprintf('Configuration 18 \n');
        p = [1.0  1.0 0.4    0.4   ];
        r = [1.0  2.0 1.0625 0.5197];
        u = [0.0  0.0 0.0    0.0   ];
        v = [1.0 -0.3 0.2145 0.2741];
        
    case{19} % Configuration 19
        fprintf('Configuration 19 \n');
        p = [1.0  1.0 0.4     0.4   ];
        r = [1.0  2.0 1.0625  0.5197];
        u = [0.0  0.0 0.0     0.0   ];
        v = [0.3 -0.3 0.2145 -0.4259];
        
    case 'Sod_x'
        fprintf('Sods Shocktube in the x-direction (2-d test) \n');
        p = [0.1   1 1 0.1  ];
        r = [0.125 1 1 0.125];
        u = [0     0 0 0    ];
        v = [0     0 0 0    ];
        
    case 'Sod_y'
        fprintf('Sods Shocktube in the y-direction (2-d test) \n');
        p = [1 1 0.1   0.1  ];
        r = [1 1 0.125 0.125] ;
        u = [0 0 0     0    ];
        v = [0 0 0     0    ];
        
    case 'constant'
        fprintf('constant state (2-d test) \n');
        p = [0.1   0.1   0.1   0.1  ];
        r = [0.125 0.125 0.125 0.125] ;
        u = [0     0     0     0    ];
        v = [0     0     0     0    ];
        
    otherwise
        error('only 18 cases are available');
end
%% Print configuration of selected IC
fprintf('\n');
fprintf('          reg 1 reg 2  reg 3  reg 4\n');
fprintf('density : %2.4f %2.4f %2.4f %2.4f \n',r);
fprintf('  x-vel : %2.4f %2.4f %2.4f %2.4f \n',u);
fprintf('  y-vel : %2.4f %2.4f %2.4f %2.4f \n',v);
fprintf('Presure : %2.4f %2.4f %2.4f %2.4f \n',p);
fprintf('\n');

%% Load Selected case Initial condition:

% Parameters of regions dimensions
reg1 = (x>=0.5 & y>=0.5); % region 1
reg2 = (x <0.5 & y>=0.5); % region 2
reg3 = (x <0.5 & y <0.5); % region 3
reg4 = (x>=0.5 & y <0.5); % region 4

% Initial Condition for our 2D domain
r_0 = r(1)*reg1 + r(2)*reg2 + r(3)*reg3 + r(4)*reg4; % Density, rho
u_0 = u(1)*reg1 + u(2)*reg2 + u(3)*reg3 + u(4)*reg4; % velocity in x
v_0 = v(1)*reg1 + v(2)*reg2 + v(3)*reg3 + v(4)*reg4; % velocity in y
p_0 = p(1)*reg1 + p(2)*reg2 + p(3)*reg3 + p(4)*reg4; % temperature.

end


