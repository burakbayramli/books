function [f]=element_load_vector_centre_2D(b_c,dof_el_v,gauss,J)

% Element load vector
n_gauss=length(gauss);
n_sd=2;
f=zeros(dof_el_v*n_sd,1);
for a=1:dof_el_v
    for i=1:n_sd
        r=n_sd*(a-1)+i;
        for n=1:n_gauss
            f(r,1)=f(r,1)+(gauss(n).N_v(a).N)*gauss(n).w;
        end
        f(r,1)=b_c(i)*f(r,1)*J;
    end
end

end