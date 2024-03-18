function [handles] = dissipation(handles)
% nnodes = handles.Data.Nodes;
p    = handles.Data.p;
vol  = handles.Data.vol;
dt   = handles.Data.dt;
vis2 = handles.Data.Vis2Explicit;
vis4 = handles.Data.Vis4Explicit;
diss = handles.Data.diss;
W    = handles.Data.W;
% nrk  = handles.Data.nrk;

dp = abs((p(3:end)-2*p(2:end-1)+p(1:end-2))./(p(3:end)+2*p(2:end-1)+p(1:end-2)));
dp = [dp(1); dp; dp(end)];
dp = 0.5*(dp(2:end)+dp(1:end-1));
% pmax = max(dp(2:end-1));

%Vectors of artificial viscosity contribution. 
eps2 = vis2*dp(2:end-1); 
eps4 = max(0,vis4-eps2);

%!!!!!!!!!!!!!!!!!!!!!1
eps2=[eps2 eps2 eps2];
eps4=[eps4 eps4 eps4];

evalflux=0.5*(vol(1:end-1)./dt(1:end-1)+vol(2:end)./dt(2:end));

% The artificial dissipation for the physical volumes
d=diag(evalflux(2:end-1))*(eps2.*(W(3:end-1,:)-W(2:end-2,:))-eps4.*(W(4:end,:)-3*W(3:end-1,:)+3*W(2:end-2,:)-W(1:end-3,:)));
% Added dissipation for "ghost" volumes
d = [d(2,:);d(1,:); d; d(end,:); d(end-1,:)];

%beta1=1-beta(nrk);

% ????????? Vad r detta ??? och vilka index skall det var ?
%diss = beta(nrk)*(d(2:end,:)-d(1:end-1,:))+beta1*diss(1:end,:);
%diss=(d(2:end,:)-d(1:end-1,:));

handles.Data.diss = diff(d);
