%**************************************************************************
%       mesh function, mesh generator and its derivatives
%**************************************************************************
%
% The mesh and its derivatives are generated with the....
%
%  Bottom wall                   Channel center                  Top wall
%     y=0                           y=H/2                         y=H
%       || |  |   |    |      |      |      |      |    |   |  | ||
%
%
% Inputs:
%   H        channel height
%   n        number of mesh points (nodes)
%   fact     factor to set mesh clustering at the wall
%   discr    discretization
%
% Output:
%   MESH     mesh structure containing: 
%            'y'      ... y coordinates
%            'ddy'    ... first derivative coefficient matrix d()/dy
%            'd2dy2'  ... second derivative coefficient matrix d2()/dy2
%

function [MESH] = mesh(H, n, fact, ns, discr)

% H = 2; 
% n = 100; 
% fact = 6.0; 
% ns = 3; 
% discr = 'chebyshev';

    switch discr
        
        case 'chebyshev'
    
            [y,coeff] = ChebyshevCoeff(n,2);
            y         = H*(y(end:-1:1)+1)/2;
            ddy       = H/2*coeff(end:-1:1,end:-1:1,1);
            d2dy2     = H/2*coeff(end:-1:1,end:-1:1,2);
        
        case 'finitediff'
            
            di = 1.0/(n-1);
            i = ((0:n-1)')/(n-1) - 0.5;

            % y - coordinate: tanh clustering at the walls
            y = H * (1.0 + tanh(fact*i)/tanh(fact/2))/2.0;
            y = (y + (2-y(end:-1:1)))/2.0;

            % coordinate transformation: derivative of y with respect to 'i'
            dydi =  H * fact/2.0/tanh(fact/2)./cosh(fact*i).^2.0;
            dydi = (dydi + dydi(end:-1:1))/2.0;
            
            % coordinate transformation: second derivative of y with respect to 'i'
            d2ydi2 = -H * fact^2.0/tanh(fact/2).*tanh(fact*i)./cosh(fact*i).^2.0;
            d2ydi2 = (d2ydi2-d2ydi2(end:-1:1))/2.0;

            % -------------------------------------------------------------
            % coefficient matrix for d()/dy
            % du/dy = 1/(dy/di) * du/di
            ddy = zeros(n,n);
            
            ddy(1,1:7) = finiteDiffCoeff( 0:6, 1);
            ddy(2,1:7) = finiteDiffCoeff(-1:5, 1);
            ddy(3,1:7) = finiteDiffCoeff(-2:4, 1);
            ddy(n-2,n-6:n) = finiteDiffCoeff(-4:2, 1);
            ddy(n-1,n-6:n) = finiteDiffCoeff(-5:1, 1);
            ddy(n,  n-6:n) = finiteDiffCoeff(-6:0, 1);   
            
%             ns = 1;
            for i=1+ns:n-ns
                ddy(i,:) = 0.0;
                ddy(i,i-ns:i+ns) = finiteDiffCoeff(-ns:ns, 1);
            end
            
            ddy = sparse(ddy);
            
            % multiply coordinate transformation 
            ddy = bsxfun(@times, ddy, 1/di./dydi);

            
            % -------------------------------------------------------------
            % coefficient matrix for d2()/dy2 (second derivative)
            % d2u/dy2 = 1/(dy/di)^2*d2u/di2 - 1/(dy/di)^3*d2y/di2*du/di
            d2dy2 = zeros(n,n);
            
            d2dy2(1,1:7) = finiteDiffCoeff( 0:6, 2);
            d2dy2(2,1:7) = finiteDiffCoeff(-1:5, 2);
            d2dy2(3,1:7) = finiteDiffCoeff(-2:4, 2);
            d2dy2(n-2,n-6:n) = finiteDiffCoeff(-4:2, 2);
            d2dy2(n-1,n-6:n) = finiteDiffCoeff(-5:1, 2);
            d2dy2(n,  n-6:n) = finiteDiffCoeff(-6:0, 2);

%             ns = 1;
            for i=1+ns:n-ns
                d2dy2(i,:) = 0.0;
                d2dy2(i,i-ns:i+ns) = finiteDiffCoeff(-ns:ns, 2);
            end
            
            d2dy2 = sparse(d2dy2);
            
            % multiply coordinate transformation 
            d2dy2 =  bsxfun(@times, d2dy2, 1/di^2./dydi.^2) ...
                    -bsxfun(@times, ddy, d2ydi2./dydi.^2);
    end
    
    MESH = struct('y',y,'ddy',ddy,'d2dy2',d2dy2);


%     % testing derivatives, plot results
%     freq = 1.3*pi;
%     phi = 0.3;
%     f  = 0.4*cos(freq*y + phi);
%     df = -0.4*freq*sin(freq*y + phi);
%     ddf = -0.4*freq^2*cos(freq*y + phi);
% 
%     figure(2); hold off 
%     plot(y, f,'ro-'); hold on
%     plot(y, df,'b-'); hold on
%     plot(y, ddf,'k-'); hold on
% 
%     df = ddy*f;
%     plot(y, df,'bo'); hold on
% 
%     ddf = d2dy2*f;
%     plot(y, ddf,'ko'); hold on
%     
%     axis([0 0.021 -6.45 -6.2])

end
    

