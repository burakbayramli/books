function [h]=assemble_load_vector_p(el,dof_p,n_el,dof_el_p,A_p)

% Assemblare of load vector for pressure
h=zeros(dof_p,1);
for n=1:n_el
    for k=1:dof_el_p
        h(A_p(n,k),1)=h(A_p(n,k),1)+el(n).h(k,1);
    end
end

end