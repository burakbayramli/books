% MG additions: Levmax, Nodes, dt, a, vol, W, p, Wold
% aug 2017
function handles = RK_MG(handles,nstp,level)
% W,p = f(gamma,R,Vis2,Vis4,p01,t01,p2,nrk,ark,CFLNumber; Nodes,dt,a,vol,W)
% update
%handles.Data.Wold{level} = handles.Data.W{level};
% thermodyn
gamma = handles.Data.gamma;
R     = handles.Data.R;
cpgas = R*gamma/(gamma-1);

% alg. parameters
nrk = handles.Data.nrk;
ark = handles.Data.ark;
cfl = handles.Data.CFLNumber;
vis2 = handles.Data.Vis2Explicit;
vis4 = handles.Data.Vis4Explicit;
% BC data
p01 = handles.Data.p01;
t01 = handles.Data.t01;
p2  = handles.Data.p2;

% level dependent variables
ib2 = handles.Data.Nodes{level} -1;
dt  = handles.Data.dt{level};

% geometry
a   = handles.Data.a{level};
vol = handles.Data.vol{level};

% state vectors

W    = handles.Data.W{level};
Wold = W;
% extra for MG
xtraf = handles.Data.XtraF{level};
for istp = 1:nstp
    for irk = 1:nrk
        % Gary-type RK: wk+1 = wold + dt*a_k*f(wk)
        [W,rhs,p] = residfun(gamma, a, W, p01, p2, t01, vis2, vis4, R, vol, dt);   
        % for MG, extra source function
        rhs = rhs - xtraf;
        adtv=ark(irk)*cfl*dt./vol;
        tmp =(adtv*[1 1 1]).*rhs;
        % update W
        W(2:ib2,:) = Wold(2:ib2,:) - tmp(2:ib2,:);  
    end
    Wold = W;
end
handles.Data.p{level} = p;
handles.Data.W{level} = W;

