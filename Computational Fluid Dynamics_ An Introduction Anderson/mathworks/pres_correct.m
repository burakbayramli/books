function [pressure,p_prime] = pres_correct(imax,jmax,rhsp,Ap,p,alpha)
pressure = p;                                                               %   p = Pressure
p_prime = zeros(imax,jmax);                                                 %   pressure correction 

p_prime_interior = pentaDiag_solve(Ap,rhsp);

%convert pressure correction in to a matrix
%update preesure values
z=1; 
for j=1:jmax
    for i=1:imax
        p_prime(i,j)=p_prime_interior(z); 
        z=z+1;
        pressure(i,j) = p(i,j) + alpha*p_prime(i,j);
    end
end
pressure(1,1) = 0;

return
end


