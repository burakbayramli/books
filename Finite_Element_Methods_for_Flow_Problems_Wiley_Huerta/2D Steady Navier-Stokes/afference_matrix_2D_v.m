function [A]=afference_matrix_2D_v(n_np_x,n_np_y,dof_el)

% Afference matrix for velocity
n_el_x=(n_np_x-1)/2;
n_el=(n_np_x-1)/2*(n_np_y-1)/2;
n_sd=2;
A=zeros(n_el,dof_el*n_sd);
A_aus=zeros(n_el,dof_el);
for i=1:n_el
    [r,c]=row_column(i,n_el_x);
    A_aus(i,1)=      r*2*n_np_x+1+2*(c-1);
    A_aus(i,2)=      r*2*n_np_x+3+2*(c-1);
    A_aus(i,3)=  (r-1)*2*n_np_x+3+2*(c-1);
    A_aus(i,4)=  (r-1)*2*n_np_x+1+2*(c-1);
    A_aus(i,5)=      r*2*n_np_x+2+2*(c-1);
    A_aus(i,6)=(r-1/2)*2*n_np_x+3+2*(c-1);
    A_aus(i,7)=  (r-1)*2*n_np_x+2+2*(c-1);
    A_aus(i,8)=(r-1/2)*2*n_np_x+1+2*(c-1);
    A_aus(i,9)=(r-1/2)*2*n_np_x+2+2*(c-1);
    
    for j=1:dof_el
        A(i,2*j-1:2*j)=[A_aus(i,j)*2-1,A_aus(i,j)*2];
    end
    
end

end