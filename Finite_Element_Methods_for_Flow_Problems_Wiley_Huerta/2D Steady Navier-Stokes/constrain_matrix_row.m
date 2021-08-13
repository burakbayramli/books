function [A_f,A_p]=constrain_matrix_row(A,dof_constrained_R)

% Constrain a matrix (only the rows)
[R,C]=size(A);
p_R=dof_constrained_R;
f_aus_R=1:R;
p_aus_R=zeros(1,R);
p_aus_R(p_R)=p_R;
f_R=f_aus_R-p_aus_R;
f_R=find(f_R);

A_f=A(f_R,:);
A_p=A(p_R,:);

end