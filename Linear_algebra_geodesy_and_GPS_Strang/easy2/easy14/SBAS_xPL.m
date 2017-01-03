% Function computes SBAS protection levels
%
% In EGNOS function we can not decide which satellites should
% be included in HPL and VPL computation, because only user can decide which
% satellites he/she want to use. So, this function will be an add-on,
% which will help user to compute protection levels only for those satellites
% which he/she is going to use for navigation or other purposes. 
%
% Input:
%   el        - elevation angles for all satellites (min 4) [radians]
%   az        - azimuth angles for all satellites (min 4)   [radians]
%               Note: positive azimuth is defined clockwise from North 
%   sigma     - sigma2_flt + sigma2_tropo + sigma2_iono + sigma2_air for all
%               satellites (min 4). From EGNOS
%
%  NOTE: all input parameters are of the same size and should include data
%  for all satellites user wants to use for protection level computation.
%
% Output:
%   HPL_prec  - Horizontal Protection Level for precision mode
%   HPL_nprec - Horizontal Protection Level for non-precision mode
%   VPL       - Vertical Protection Level
%  
%  NOTE: if all parameters =0, something may be wrong

% By Kostas Dragunas
% date: 2008 03 11
% last edited: 2008 06 27

function [HPL_prec HPL_nprec VPL] = SBAS_xPL(el, az, sigma)

% initialize data
HPL_prec  = 0; 
HPL_nprec = 0; 
VPL       = 0;

% number of satellites
sat_nr = size(el,2);
% check if there are enogh satellites
if sat_nr>4
    % G matrix (RTCA do229c page J-2)
    G = ones(sat_nr,4);
    for i = 1:sat_nr
        G(i,1) = cos(el(i))*cos(az(i));
        G(i,2) = cos(el(i))*sin(az(i));
        G(i,3) = sin(el(i));
        % G(i,4) = 1; % this one is defined with G = ones(sat_nr,4);
    end

    % W matrix (RTCA do229c page J-2)
    W        = diag(1./sigma);

    D        = inv(G'*W*G);
    d2_east  = D(1,1); % we do not do sqrt, since later we need it squared
    d_EN     = D(1,2);
    d2_north = D(2,2); % we do not do sqrt, since later we need it squared
    d_U      = sqrt(D(3,3));

    % K_H for HPL (J.2.1)
    % for non-precision approach
    K_H_nprec = 6.18;
    % for precision approach
    K_H_prec  = 6.0;
    % K_V for VPL 
    K_V       = 5.33;

    % d_major (J.1)
    d_major  = sqrt((d2_east+d2_north)/2 + sqrt(((d2_east-d2_north)/2)^2+d_EN^2));

    % HPL non-precision approach
    HPL_nprec  = K_H_nprec * d_major;
    % HPL precision approach
    HPL_prec   = K_H_prec  * d_major;
    % VPL
    VPL        = K_V       * d_U;
end
