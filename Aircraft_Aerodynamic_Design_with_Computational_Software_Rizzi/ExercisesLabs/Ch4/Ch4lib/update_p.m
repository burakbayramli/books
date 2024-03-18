function [handles] = update_p(handles)
ib2 = handles.Data.Nodes - 1;
a = handles.Data.a;
gamma = handles.Data.gamma;
W = handles.Data.W;

rrho     = a./W(:,1);
rhou     = W(:,2)./a;
rhoe     = W(:,3)./a;

p = (gamma-1).*(rhoe-0.5.*rhou.^2.*rrho);

handles.Data.p = p;

