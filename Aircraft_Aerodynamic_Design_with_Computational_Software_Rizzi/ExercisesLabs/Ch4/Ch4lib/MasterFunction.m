function [rhs]=MasterFunction(t,W);

[m,n]=size(W);
W=reshape(W,round(m/3),3);

global ImpVar

handles.Data=ImpVar;
handles.Data.W = W;
handles.Data.Vis2Explicit = handles.Data.Vis2Implicit;
handles.Data.Vis4Explicit = handles.Data.Vis4Implicit;
if ImpVar.roe

    % Update pressure.
    handles = update_p(handles);
    % Update boundary conditions.
    handles = bcond(handles);
    % Calculated left and right states for each node.
    handles = lr_state(handles);
    % Form residual due to Roe.[p,W] = bcond(ib2,a,p,p01,t01,p2,gamma,R,W);
    handles = flux_roe(handles);
    % Add source term to right hand side.
    handles = srcterm(handles);
    % Residual * timestep.
    %[rhs] = res_tsimpl(ImpVar.ib2,ImpVar.cfl,ImpVar.dt,ImpVar.vol,ImpVar.rhs);
    % P/I Implicit residula smoothing.
    %irsmoo;

    rhs=-[handles.Data.rhs(:,1); handles.Data.rhs(:,2); handles.Data.rhs(:,3)];



else

    % Update pressure.
    nrk=1; beta=1;diss=1;

    handles = update_p(handles);
    % Update boundary conditions.
    handles = bcond(handles);

    handles = tstep(handles);
    % calculatets the dissipation
    handles = dissipation(handles);
    % calculates the central flux
    handles = flux_center(handles);
    % Add source term to right hand side.
    handles = srcterm(handles);
    % Residual * timestep.
    %[rhs] = res_ts(ib2,ark,irk,cfl,kex,vol,rhs);
    % P/I Implicit residula smoothing.
    %[rhs] = irsmoo(nnodes,ib2,rhs,epsirs);
    % Update conservative variables.
    %[W] = update_W(ib2,rhs,W,Wold);

    rhs=-[handles.Data.rhs(:,1); handles.Data.rhs(:,2); handles.Data.rhs(:,3)];
end
% else
%     disp('Unknown scheme');
%     return
% end