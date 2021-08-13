function [r,c]=row_column(n,n_el_x)

% Definition of the row and the column of a finite element
r=ceil(n/n_el_x);
if mod(n,n_el_x)~=0
    c=mod(n,n_el_x);
else
    c=n_el_x;
end

end