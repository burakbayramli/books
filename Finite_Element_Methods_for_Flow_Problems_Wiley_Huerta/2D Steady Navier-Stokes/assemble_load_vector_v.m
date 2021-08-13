function [f]=assemble_load_vector_v(el,dof_v,n_el,dof_el_v,A_v)

% Assemblare of load vector for velocity
n_sd=2;
f=zeros(dof_v*n_sd,1);
for n=1:n_el
    for a=1:dof_el_v
        for i=1:n_sd
            r=n_sd*(a-1)+i;
            f(A_v(n,r),1)=f(A_v(n,r),1)+el(n).f(r,1);
        end
    end
end

end