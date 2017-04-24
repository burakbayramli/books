% -----------------------------------------------------------------------------
%
%                           function getgravc
%
%  this function gets constants for the propagator. note that mu is identified to
%    facilitiate comparisons with newer models.
%
%  author        : david vallado                  719-573-2600   21 jul 2006
%
%  inputs        :
%    whichconst  - which set of constants to use  721, 72, 84
%
%  outputs       :
%    tumin       - minutes in one time unit
%    mu          - earth gravitational parameter
%    radiusearthkm - radius of the earth in km
%    xke         - reciprocal of tumin
%    j2, j3, j4  - un-normalized zonal harmonic values
%    j3oj2       - j3 divided by j2
%
%  locals        :
%
%  coupling      :
%
%  references    :
%    norad spacetrack report #3
%    vallado, crawford, hujsak, kelso  2006
% [tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2] = getgravc(whichconst);
%  --------------------------------------------------------------------------- */

function [tumin, mu, radiusearthkm, xke, j2, j3, j4, j3oj2] = getgravc(whichconst);

   global tumin mu radiusearthkm xke j2 j3 j4 j3oj2  
      switch whichconst
          case 721
           % -- wgs-72 low precision str#3 constants --
           mu     = 398600.79964;        %// in km3 / s2
           radiusearthkm = 6378.135;     %// km
           xke    = 0.0743669161;
           tumin  = 1.0 / xke;
           j2     =   0.001082616;
           j3     =  -0.00000253881;
           j4     =  -0.00000165597;
           j3oj2  =  j3 / j2;
          case 72
           % ------------ wgs-72 constants ------------
           mu     = 398600.8;            %// in km3 / s2
           radiusearthkm = 6378.135;     %// km
           xke    = 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  = 1.0 / xke;
           j2     =   0.001082616;
           j3     =  -0.00000253881;
           j4     =  -0.00000165597;
           j3oj2  =  j3 / j2;
          case 84
           % ------------ wgs-84 constants ------------
           mu     = 398600.5;            %// in km3 / s2
           radiusearthkm = 6378.137;     %// km
           xke    = 60.0 / sqrt(radiusearthkm*radiusearthkm*radiusearthkm/mu);
           tumin  = 1.0 / xke;
           j2     =   0.00108262998905;
           j3     =  -0.00000253215306;
           j4     =  -0.00000161098761;
           j3oj2  =  j3 / j2;
          otherwise
           fprintf('unknown gravity option (%d)\n',whichconst);
      end;  % case

