function [handles] = InitEmpty_MG(handles)
% JO 170901
levmax = handles.Data.Levmax;
% Initiates needed empty variables
handles.Data.dthist   = [];
handles.Data.tocold   = 0;                 % Variable to help to compute the total elepsed time
handles.Data.iterold  = 0;                 % variable to help compute the total mumbers of iterations.
handles.Data.iter     = 0;                 % Index variables for controlling the itterationprocess
handles.Data.iplot    = 0;                 % Index variables for controlling the plotting
handles.Data.drho     = 1;
handles.Data.convhist = [];                % Used to store convergence history.
handles.Data.drhoold  = [];                % The initial drho.

% Variables sized for multigrid:
x     = cell(levmax,1);
a     = cell(levmax,1);
Wold  = cell(levmax,1);
diss  = cell(levmax,1);
ib2   = cell(levmax,1);
Nodes = cell(levmax,1);
dxref = cell(levmax,1);
dt    = cell(levmax,1);
XtraF = cell(levmax,1);
for k = 1:levmax
    nnodes  = (handles.Data.Nodes0-3)*2^(k-1)+3
    Nodes{k}= nnodes;            % Number of physical nodes.
    ib2{k}  = nnodes-1;          % Last "physical" grid point.
    x{k}    = zeros(nnodes,1);   % x-coordinate of each node [m].
    a{k}    = zeros(nnodes,1);   % Local area at nodes [m^2].
    Wold{k} = zeros(nnodes,3);   % Solution from previous step.
    diss{k} = zeros(nnodes,3);   % Vectior containing artificial dissipation
    dt{k}   = zeros(nnodes,1);   % local time steps
    XtraF{k}   = zeros(nnodes,3);   % extra source function for MG FAS
end
handles.Data.Nodes = Nodes;
handles.Data.x     = x;
handles.Data.a     = a  ;
handles.Data.Wold  = Wold;
handles.Data.diss  = diss;
handles.Data.ib2   = ib2;
handles.Data.dxref = dxref;
handles.Data.dt    = dt;
handles.Data.XtraF = XtraF;

