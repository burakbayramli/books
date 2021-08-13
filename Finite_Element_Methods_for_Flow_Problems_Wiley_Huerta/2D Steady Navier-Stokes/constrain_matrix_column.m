function [A_f,A_p]=constrain_matrix_column(A,dof_constrained_C)

% Constrain a matrix (only the columns)
[R,C]=size(A);
p_C=dof_constrained_C;
f_aus_C=1:C;
p_aus_C=zeros(1,C);
p_aus_C(p_C)=p_C;
f_C=f_aus_C-p_aus_C;
f_C=find(f_C);

A_f=A(:,f_C);
A_p=A(:,p_C);

end