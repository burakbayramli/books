% This code accompanies
%   The Lattice Boltzmann Method: Principles and Practice
%   T. Krüger, H. Kusumaatmaja, A. Kuzmin, O. Shardt, G. Silva, E.M. Viggen
%   ISBN 978-3-319-44649-3 (Electronic) 
%        978-3-319-44647-9 (Print)
%   http://www.springer.com/978-3-319-44647-9
%
% This code is provided under the MIT license. See LICENSE.txt.
%
% Author: Orest Shardt
% 
% exact solution for Taylor-Green vortex decay
%

function [rho,u,v,P] = taylorgreen(t,X,Y,NX,NY,nu,rho0,u_max)
    kx = 2*pi/NX;
    ky = 2*pi/NY;
    td = 1/(nu*(kx*kx+ky*ky));
    
    u = -u_max*sqrt(ky/kx)*cos(kx*X).*sin(ky*Y)*exp(-t/td);
    v =  u_max*sqrt(kx/ky)*sin(kx*X).*cos(ky*Y)*exp(-t/td);
    P = -0.25*rho0*u_max*u_max*((ky/kx)*cos(2*kx*X)+(kx/ky)*cos(2*ky*Y))*exp(-2*t/td);
    rho = rho0+3*P;
