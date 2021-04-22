function[F] = form_beam_F(F)
%
% This function assembles the joints loads 
% to the global force vector
%
global nnd nodof nel eldof
global nf Joint_loads Element_loads
%
for i=1:nnd
    for j=1:nodof
        if nf(i,j)~= 0
           F(nf(i,j)) = Joint_loads(i,j);
        end
    end
end
%
%
for i=1:nel
g=beam_g(i) ; % Retrieve the element steering vector
for j=1:eldof
    if g(j)~= 0
        F(g(j))= F(g(j)) + Element_loads(i,j);
    end
end
end
%%%%%%%%% End function form_beam_F %%%%%%%%%%%%%%%%
