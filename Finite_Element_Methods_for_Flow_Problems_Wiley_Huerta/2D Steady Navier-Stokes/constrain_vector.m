function [v_f,v_p]=constrain_vector(v,dof_constrained)

% Constrain a vector
N=length(v);
p=dof_constrained;
f_aus=1:N;
p_aus=zeros(1,N);
p_aus(p)=p;
f=f_aus-p_aus;
f=find(f);

v_f=v(f);
v_p=v(p);

end