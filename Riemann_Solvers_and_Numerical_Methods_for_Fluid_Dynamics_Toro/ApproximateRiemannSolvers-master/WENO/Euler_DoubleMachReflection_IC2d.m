function [r0,u0,v0,p0,preshock,postshock,shockSpeed] = Euler_DoubleMachReflection_IC2d(nx,ny)
    % Load the IC of a classical Double Mach Reflection test problem.
    %
    %  r = rho/gas density
    %  u = velocity in x direction
    %  v = velocity in y direction
    %  p = Pressure
    %
    %  Adiabatic gas with γ = 1.4
    %
    %  Recall that Rankine Hugoniot equations relates the conditions ahead
    %  of and behing of a moving shock,
    %
    %   p2/p1 = { 2*gamma*Ms^2-(gamma-1) } / (gamma+1) ,
    %   r2/r1 = { Tau*(p2/p1) + 1 } / { Tau + (p2/p1) } ,
    %   u2/c1 = Ms*{ 1-((gamma-1)*Ms^2 +2)/((gamma+1) Ms^2) } ,
    %
    %  where:  Tau = (gamma+1)/(gamma-1) and c1 = (gamma*p1/rho1)^(1/2)
    %
    %  Coded by Manuel A. Diaz, manuel.ade'at'gmail.com 
    %     Institute of Applied Mechanics, 2012.09.06
    %
    global gamma
    fprintf('Double Mach Reflection\n');

    % state 1, ahead of the moving shock,
    r1 = 1.4;
    u1 = 0.0;
    v1 = 0.0;
    p1 = 1.0;
    E1 = p1./((gamma-1))+0.5*r1*(u1.^2+v1.^2);
    preshock = [r1,r1*u1,r1*v1,E1];

    % state 2, behing of the shock:
    % we consider a Mach=10 shock at a 30 [deg] angle.

    % Set Ms ,Tau and c1,
    Ms = 10; c1 = sqrt(gamma*p1/r1); Tau = (gamma+1)/(gamma-1);
    % set properties
    p2 = p1*(2*gamma*Ms^2-(gamma-1))/(gamma+1);
    r2 = r1*(Tau*(p2/p1) + 1)/(Tau + (p2/p1));
    u2 = Ms*(1-((gamma-1)*Ms^2 + 2)/((gamma+1)*Ms^2))*c1*cos(pi/6);
    v2 =-Ms*(1-((gamma-1)*Ms^2 + 2)/((gamma+1)*Ms^2))*c1*sin(pi/6);
    E2 = p2./((gamma-1))+0.5*r2*(u2.^2+v2.^2);
    postshock = [r2,r2*u2,r2*v2,E2]; shockSpeed=Ms/cos(pi/6);

    % build grid
    dx=4/nx; dy=1/ny; [x,y]=meshgrid(dx/2:dx:4,dy/2:dy:1);

    % Initial position of the shock
    x0=1/6;

    % Undisturbed gas state
    r0 = r1*ones(ny,nx);
    u0 = u1*ones(ny,nx);
    v0 = v1*ones(ny,nx);
    p0 = p1*ones(ny,nx);

    % Shock state
    mask = x<x0+y*tan(pi/6); % tan(pi/6)=1/sqrt(3);
    r0(mask) = r2;
    u0(mask) = u2;
    v0(mask) = v2;
    p0(mask) = p2;

end


