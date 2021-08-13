function [u]=data_all_dof(u_f,u_p,dof_constrained)

% Data for all DOF
N=length(u_f)+length(u_p);
u=zeros(N,1);

p=dof_constrained;
f_aus=1:N;
p_aus=zeros(1,N);
p_aus(p)=p;
f=f_aus-p_aus;
f=find(f);

u(f)=u_f;
u(p)=u_p;

end