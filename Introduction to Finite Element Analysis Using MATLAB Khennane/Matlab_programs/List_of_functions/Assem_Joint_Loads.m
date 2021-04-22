function[F] = Assem_Joint_Loads(F)
%
% This function assembles the joints loads 
% to the global force vector
%
global nnd nodof 
global nf Joint_loads 
%
for i=1:nnd
    for j=1:nodof
        if nf(i,j)~= 0
           F(nf(i,j)) = Joint_loads(i,j);
        end
    end
end
end
%%%%%%%%% End function form_beam_F  %%%%%%%%%%%%%%%%