function [handles] = InitEmpty(handles)

% Initiates needed empty variables
handles.Data.dthist   = [];
handles.Data.tocold   = 0;                 % Variable to help to compute the total elepsed time
handles.Data.iterold  = 0;                 % variable to help compute the total mumbers of iterations.
handles.Data.iter     = 0;                 % Index variables for controlling the itterationprocess
handles.Data.iplot    = 0;                 % Index variables for controlling the plotting
handles.Data.drho     = 1;
handles.Data.convhist = [];                % Used to store convergence history.
handles.Data.drhoold  = [];                % The initial drho.
handles.Data.tim      = 0;                 % actual time JO  1904
handles.Data.meth     = 'eroe';            % default explicit Roe
 
nnodes = handles.Data.Nodes;
Nodes = nnodes;           % Number of physical nodes.
ib2  = nnodes-1;          % Last "physical" grid point.
x    = zeros(nnodes,1);   % x-coordinate of each node [m].
a    = zeros(nnodes,1);   % Local area at nodes [m^2].
Wold = zeros(nnodes,3);   % Solution from previous step.
diss = zeros(nnodes,3);   % Vectior containing artificial dissipation
dt   = zeros(nnodes,1);   % local time steps
XtraF   = zeros(nnodes,3);   % extra source function for MG FAS
handles.Data.Nodes = Nodes;
handles.Data.x     = x;
handles.Data.a     = a  ;
handles.Data.Wold  = Wold;
handles.Data.diss  = diss;
handles.Data.ib2   = ib2;
%handles.Data.dxref = dxref;
handles.Data.dt    = dt;
handles.Data.XtraF = XtraF;

