%function u = MGVcycle(u0,r,level)
function handles = MGVcycle(handles, r, level)
disp(['mg ' num2str(level)])
R     = handles.Data.R;
% alg. parameters
gamma = handles.Data.gamma;
vis2  = handles.Data.Vis2Explicit;
vis4  = handles.Data.Vis4Explicit;
% BC data
p01 = handles.Data.p01;
t01 = handles.Data.t01;
p2  = handles.Data.p2;
a   = handles.Data.a{level};
dt  = handles.Data.dt{level};
vol = handles.Data.vol{level};
% MG parameters
ncoarse = handles.Data.Ncoarse;
npre    = handles.Data.Npre;
npost   = handles.Data.Npre;
% pick solution on level
u0      = handles.Data.W{level};
figure(level)
clf
plot(u0(:,1),'.-k')
hold on
%F = f_{level}(u0) - r;
[Wdum,ftmp,pdum] = residfun(gamma,a,u0,p01,p2,t01,vis2,vis4,R,vol,dt);
F    = ftmp-r;
if level < handles.Data.Levmax
handles.Data.XtraF{level}=F;
end
if level == 1
    disp('coarse solv')
    %    u = RK^{mmax}(u0,F)         % iterate only
    handles = RK_MG(handles,ncoarse,level);   % "solve" on coarsest level
    wtmp = handles.Data.W{level};
    figure(level)
    plot(wtmp(:,1),'.-r')
else
    %   u   = RK^mpre(u0,F)       % pre-smooth
    handles = RK_MG(handles,npre,level);
    u       = handles.Data.W{level};
    disp([' pre: ' num2str(norm(u(:,1)-u0(:,1)))])
    %   r = f_{level}(u)
    [Wdum,r,pdum] = residfun(gamma,a,u,p01,p2,t01,vis2,vis4,R,vol,dt);
    %   uC0 = R*u                   % restrict solution
    uC0 = rstrct(u);
    %   rC  = R*r                   % and residual
    rC  = rstrct(r);
    handles.Data.W{level-1} = uC0;
    handles = MGVcycle(handles,rC,level-1); % send to coarser level
    uC1     = handles.Data.W{level-1};
    disp(['post : ',num2str(norm(uC1(:,1)-uC0(:,1)))])
    u   = u + prlng(uC1-uC0);  % correct
    %  u   = RK^{mpost}(u,F)   % post-smooth
    figure(level)
    plot(u(:,1),'.-r')
    hold on
    handles.Data.W{level}=u;
    handles = RK_MG(handles,npost,level);
end
