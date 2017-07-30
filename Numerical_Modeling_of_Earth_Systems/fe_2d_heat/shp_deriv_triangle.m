function  [N, dNdu] = shp_deriv_triangle(ipx, nnodel)
%SHP_DERIV_TRIANGLE Shape functions and their derivatives with respect to local coordinates
%  Supports 3,6, and 7 node triangular elements 

%   Part of MILAMIN: MATLAB-based FEM solver for large problems, Version 1.0
%   Copyright (C) 2007, M. Dabrowski, M. Krotkiewski, D.W. Schmid
%   University of Oslo, Physics of Geological Processes
%   http://milamin.org
%   See License file for terms of use.

nip  = size(ipx,1); % number of integration points

% value of form function at integration points
N    = cell(nip,1);% arrays of emtry matrices

% derivatives of form functions with respect to local coordinates
dNdu = cell(nip,1);

for i=1:nip
    % helper variables for linear triangle element
    eta2 = ipx(i,1);
    eta3 = ipx(i,2);
    eta1 = 1-eta2-eta3;

    switch nnodel;
        case 3
            SHP   = [eta1; ...
                     eta2; ...
                     eta3];
            % dN(x_i) /deta_j
            DERIV = [-1 1 0; ...   %w.r.t eta2
                     -1 0 1];      %w.r.t eta3

        case 6
            SHP = [eta1*(2*eta1-1);
                   eta2*(2*eta2-1);
                   eta3*(2*eta3-1);
                       4*eta2*eta3;
                       4*eta1*eta3;
                       4*eta1*eta2];

            DERIV = [1-4*eta1 -1+4*eta2         0 4*eta3 -4*eta3        4*eta1-4*eta2; ...   %w.r.t eta2
                     1-4*eta1         0 -1+4*eta3 4*eta2  4*eta1-4*eta3       -4*eta2];      %w.r.t eta3
             
        case 7
            SHP = [eta1*(2*eta1-1)+ 3*eta1*eta2*eta3;
                   eta2*(2*eta2-1)+ 3*eta1*eta2*eta3;
                   eta3*(2*eta3-1)+ 3*eta1*eta2*eta3;
                     4*eta2*eta3 - 12*eta1*eta2*eta3;
                     4*eta1*eta3 - 12*eta1*eta2*eta3;
                     4*eta1*eta2 - 12*eta1*eta2*eta3;
                                   27*eta1*eta2*eta3];

            DERIV = [1-4*eta1+3*eta1*eta3-3*eta2*eta3 ...
                    -1+4*eta2+3*eta1*eta3-3*eta2*eta3 ...
                              3*eta1*eta3-3*eta2*eta3 ...
                     4*eta3+12*eta2*eta3-12*eta1*eta3 ...
                    -4*eta3+12*eta2*eta3-12*eta1*eta3 ...
              4*eta1-4*eta2+12*eta2*eta3-12*eta1*eta3 ...
                           -27*eta2*eta3+27*eta1*eta3;...   %w.r.t eta2
                     1-4*eta1+3*eta1*eta2-3*eta2*eta3 ...
                             +3*eta1*eta2-3*eta2*eta3 ...
                    -1+4*eta3+3*eta1*eta2-3*eta2*eta3 ...
                     4*eta2-12*eta1*eta2+12*eta2*eta3 ...
              4*eta1-4*eta3-12*eta1*eta2+12*eta2*eta3 ...
                    -4*eta2-12*eta1*eta2+12*eta2*eta3 ...
                            27*eta1*eta2-27*eta2*eta3];     %w.r.t eta3

        otherwise
            error('Unknown element')

    end
	
    N{i} = SHP;
    dNdu{i} = DERIV';
 
end

