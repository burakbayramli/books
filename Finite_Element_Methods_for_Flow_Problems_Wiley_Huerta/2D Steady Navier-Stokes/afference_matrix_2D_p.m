function [A]=afference_matrix_2D_p(n_np_x,n_np_y,dof_el)

% Afference matrix for pressure
n_el=(n_np_x-1)*(n_np_y-1);
A=zeros(n_el,dof_el);
for i=1:n_el
    [r,c]=row_column(i,n_np_x-1);
    A(i,1)=r*n_np_x+1+(c-1);
    A(i,2)=r*n_np_x+2+(c-1);
    A(i,3)=(r-1)*n_np_x+2+(c-1);
    A(i,4)=(r-1)*n_np_x+1+(c-1);
end

end