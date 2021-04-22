function[F] = form_truss_F(F)
%
% This function forms the global force vector
%
global nnd nodof 
global nf load
%
for i=1:nnd
    for j=1:nodof
        if nf(i,j)~= 0
           F(nf(i,j)) = load(i,j);
        end
    end
end
%%%%%%%%% End function form_truss_F  %%%%%%%%%%%%%%%%