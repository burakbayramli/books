function [handles] = flux_center(handles)
nnodes = handles.Data.Nodes;
a    = handles.Data.a;
p    = handles.Data.p;
diss = handles.Data.diss;
W    = handles.Data.W;

si=0.5*(a(2:end)+a(1:end-1));

rav  = 0.5*(W(2:end,1)./a(2:end)+W(1:end-1,1)./a(1:end-1));
ruav = 0.5*(W(2:end,2)./a(2:end)+W(1:end-1,2)./a(1:end-1));
reav = 0.5*(W(2:end,3)./a(2:end)+W(1:end-1,3)./a(1:end-1));

pav =0.5*(p(2:end)+p(1:end-1));
rhav=reav+pav;

qs = ruav.*si./rav;

% Matrix of flux
f(:,1)=rav.*qs;
f(:,2)=ruav.*qs+pav.*si;
f(:,3)=rhav.*qs;


%flux + dissipation = RHS

rhs=zeros(nnodes,3);
rhs(2:end-1,1)=f(2:end,1)-f(1:end-1,1)-diss(2:end-1,1);
rhs(2:end-1,2)=f(2:end,2)-f(1:end-1,2)-diss(2:end-1,2);
rhs(2:end-1,3)=f(2:end,3)-f(1:end-1,3)-diss(2:end-1,3);

handles.Data.rhs = rhs;