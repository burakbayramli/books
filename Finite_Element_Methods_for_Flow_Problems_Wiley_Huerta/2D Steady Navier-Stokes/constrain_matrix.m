function [A_ff,A_fp,A_pf,A_pp]=constrain_matrix(A,dof_constrained_R,dof_constrained_C)

% Constrain a matrix
[R,C]=size(A);
p_R=dof_constrained_R;
p_C=dof_constrained_C;
f_aus_R=1:R;
f_aus_C=1:C;
p_aus_R=zeros(1,R);
p_aus_C=zeros(1,C);
p_aus_R(p_R)=p_R;
p_aus_C(p_C)=p_C;
f_R=f_aus_R-p_aus_R;
f_C=f_aus_C-p_aus_C;
f_R=find(f_R);
f_C=find(f_C);

A_ff=A(f_R,f_C);
A_fp=A(f_R,p_C);
A_pf=A(p_R,f_C);
A_pp=A(p_R,p_C);

end