%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOWEvO                               %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%                              By:                                  %
%                 Erik Olsson, Created Okt. 19 2005                 %
%                        Based on code by:                          %
%            Alexander von Essen, Created Jan. 18 2005              %
%                                                                   %
%                   Last modified: Okt.  19 2005
%      MG addition: Levmax, a, Nodes, W. p, dxref {1:Levmax         %
%                             Aug 2017                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: iniflow.m                                                   %
% Purpose:Initializes the flow field using total pressure and       %
%           temperature (inlet), and static pressure (outlet).      %
%           Initializes reference values of the limiter.            %
% Called by: main.m                                                 %
% Calls:                                                            %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% W,p,dxref,uref,rhoref,pref = f(gamma,p01,t01,p2,R,   a,Nodes,)
function handles = iniflow(handles)

% Parameters used to generate initial data.
gamma = handles.Data.gamma;
p01   = handles.Data.p01;
t01   = handles.Data.t01;
p2    = handles.Data.p2;
rgas  = handles.Data.R; % Specific gas konstant.
%Added:
cpgas = rgas*gamma/(gamma-1);           % Specific heat at constant pressure.
temp = t01*(p2/p01)^((gamma-1)/gamma);  % Temperature at exit - isentropic
rho  = p2/(rgas*temp);                  % Density at exit.
mach = sqrt(2*((t01/temp)-1)/(gamma-1));% Mach number at exit.
cs   = sqrt(gamma*p2/rho);              % Speed of sound at exit.
u    = cs*mach;                         % Speed at exit.
e    = (cpgas-rgas)*t01;                % Internal energy at inlet.
rhoref = rho;                           % Reference density
uref   = u;                             % Reference speed
pref   = p2;                            % Reference preassure
handles.Data.rhoref = rhoref;
handles.Data.uref   = uref;
handles.Data.pref   = pref;
nnodes = handles.Data.Nodes;
handles.Data.dxref = 1/(nnodes-1);% Distance betwen evenly distributed nodes.
a      = handles.Data.a;
if norm(a-mean(a)*ones(size(a)))> 0.001 
    % nozzle, look for steady sol.
    disp('nozzle')
    mass = rho*u*a(2);                      % Massflow at inlet.
    % Flow field & pressure
    p = linspace(p01,p2,nnodes)';
    rho01 = p01/t01/rgas;
    rhol  = rho01*(p/p01).^(1/gamma);
    handles.Data.W = [rhol.*a,mass.*ones(nnodes,1),(gamma/(gamma-1))*p.*a];
    handles.Data.p = p;
else
    nup = round(nnodes/2);
    % shock-tube, initially u = 0
    disp('shock-tube')
    aup    = a(1:nup);
    adwn   = a(nup+1:end);
    rhoup  = p01/t01/rgas;
    rhodwn = p2/t01/rgas;
    eup    = 1/(gamma-1)*p01/rhoup;
    edwn   = 1/(gamma-1)*p2/rhodwn;
    handles.Data.p = [p01*ones(nup,1);p2*ones(nnodes-nup,1)];
    handles.Data.W = [[rhoup.*aup  ,zeros(size(aup)) ,(rhoup*eup)  .*aup];...
        [rhodwn.*adwn,zeros(size(adwn)),(rhodwn*edwn).*adwn]];
end




