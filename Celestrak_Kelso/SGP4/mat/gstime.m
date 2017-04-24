% -----------------------------------------------------------------------------
%
%                           function gstime
%
%  this function finds the greenwich sidereal time (iau-82).
%
%  author        : david vallado                  719-573-2600    7 jun 2002
%
%  revisions
%                -
%
%  inputs          description                    range / units
%    jdut1       - julian date of ut1             days from 4713 bc
%
%  outputs       :
%    gst         - greenwich sidereal time        0 to 2pi rad
%
%  locals        :
%    temp        - temporary variable for reals   rad
%    tut1        - julian centuries from the
%                  jan 1, 2000 12 h epoch (ut1)
%
%  coupling      :
%
%  references    :
%    vallado       2007, 193, Eq 3-43
%
% gst = gstime(jdut1);
% -----------------------------------------------------------------------------

function gst = gstime(jdut1);

        twopi      = 2.0*pi;
        deg2rad    = pi/180.0;

        % ------------------------  implementation   ------------------
        tut1= ( jdut1 - 2451545.0 ) / 36525.0;

        temp = - 6.2e-6 * tut1 * tut1 * tut1 + 0.093104 * tut1 * tut1  ...
               + (876600.0 * 3600.0 + 8640184.812866) * tut1 + 67310.54841;

        % 360/86400 = 1/240, to deg, to rad
        temp = rem( temp*deg2rad/240.0,twopi );

        % ------------------------ check quadrants --------------------
        if ( temp < 0.0 )
            temp = temp + twopi;
          end

        gst = temp;

