function [u_star,d_u] = u_momentum_new(imax,jmax,dx,dy,rho,mu,u,v,p,velocity,alpha)

u_star=zeros(imax+1,jmax);
d_u=zeros(imax+1,jmax);

De  = mu*dy / dx;  %convective coefficients
Dw  = mu*dy / dx; 
Dn  = mu*dx / dy; 
Ds  = mu*dx / dy; 

A = @(F,D)( max(0, (1-0.1 * abs(F/D))^5 ) );

%%compute u_star
for i = 2:imax
    for j = 2:jmax-1
        Fe  = .5*rho*dy*(u(i+1,j)+u(i,j));                                     
        Fw  = .5*rho*dy*(u(i-1,j)+u(i,j)); 
        Fn  = .5*rho*dx*(v(i,j+1)+v(i-1,j+1)); 
        Fs  = .5*rho*dx*(v(i,j)+v(i-1,j));
        
        aE = De * A(Fe,De) + max(-Fe,0);
        aW = Dw * A(Fw,Dw) + max(Fw,0);
        aN = Dn * A(Fn,Dn) + max(-Fn,0);
        aS = Ds * A(Fs,Ds) + max(Fs,0);
        aP = aE + aW + aN + aS + (Fe-Fw) + (Fn-Fs);
        
        pressure_term = (p(i-1,j)-p(i,j)) * dy;
        
        u_star(i,j) = alpha/aP * ( (aE*u(i+1,j)+aW*u(i-1,j)+aN*u(i,j+1)+aS*u(i,j-1)) + pressure_term ) + (1-alpha)*u(i,j);
        
        d_u(i,j) = alpha * dy / aP;   %refer to Versteeg CFD book
        
    end
end

%%set d_u for top and bottom BCs
%%they will be later used by the pressure correction equation 
%%they should not be zero, or BCs of pressure correction will get messed up
j = 1; %bottom
for i=2:imax
    Fe  = .5*rho*dy*(u(i+1,j)+u(i,j));                                     
    Fw  = .5*rho*dy*(u(i-1,j)+u(i,j)); 
    Fn  = .5*rho*dx*(v(i,j+1)+v(i-1,j+1)); 
    Fs  = 0;
        
    aE = De * A(Fe,De) + max(-Fe,0);
    aW = Dw * A(Fw,Dw) + max(Fw,0);
    aN = Dn * A(Fn,Dn) + max(-Fn,0);
    aS = 0;
    aP = aE + aW + aN + aS + (Fe-Fw) + (Fn-Fs);
    d_u(i,j) = alpha * dy / aP;
end

j = jmax; %top
for i=2:imax
    Fe  = .5*rho*dy*(u(i+1,j)+u(i,j));                                     
    Fw  = .5*rho*dy*(u(i-1,j)+u(i,j)); 
    Fn  = 0; 
    Fs  = .5*rho*dx*(v(i,j)+v(i-1,j));
        
    aE = De * A(Fe,De) + max(-Fe,0);
    aW = Dw * A(Fw,Dw) + max(Fw,0);
    aN = 0;
    aS = Ds * A(Fs,Ds) + max(Fs,0);
    aP = aE + aW + aN + aS + (Fe-Fw) + (Fn-Fs);
    d_u(i,j) = alpha * dy / aP;
end


%%Apply BCs
u_star(1,1:jmax) = -u_star(2,1:jmax); %left wall
u_star(imax+1,1:jmax) = -u_star(imax,1:jmax); %right wall
u_star(1:imax+1, 1) = 0.0; %bottom wall
u_star(1:imax+1, jmax) = velocity; %top wall 


return 
end