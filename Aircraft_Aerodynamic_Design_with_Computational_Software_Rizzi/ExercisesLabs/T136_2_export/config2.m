function output = config2(input,thomedir,datahomedir,afdir)
% Sets/gets reference quantities and directory structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1999, 2007 Tomas Melin
%
% This file is part of Tornado
%
% Tornado is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public
% License as published by the Free Software Foundation;
% either version 2, or (at your option) any later version.
%
% Tornado is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied
% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE.  See the GNU General Public License for more
% details.
%
% You should have received a copy of the GNU General Public
% License along with Tornado; see the file GNU GENERAL
% PUBLIC LICENSE.TXT.  If not, write to the Free Software
% Foundation, 59 Temple Place -Suite 330, Boston, MA
% 02111-1307, USA.
%
% usage: [output] = config(input,x,y,...)
%
% This function sets some reference units in Tornado. For example:
% If the user doesn't wasn the reference area to be computed within
% Tornado, this is the file to set it in.
%
% Example:
%
%  S_ref=config('S_ref')
%
%  in case 'S_ref' is unspecified in config.m, the function will return
%  an empty matrix [].
%
% Calls:
%           none
%
% Author:   Tomas Melin <melin@kth.se>
% Keywords: Tornado core function.
%
% Revision History:
%   KTH 0908             add save directory settings also for case 1 & 2
%   KTH, 090614          JOp add parameters thomedir, datahomedir,
%                        option 'getdir' for managing project directory vs.
%                        m-file home and airfoil library
%   Bristol, 2007-12-15:  Addition of output directory configuration. TM
%   Bristol, 2007-07-25:  Addition of new header. TM
%   Bristol, 2007-06-20:  Addition of Verbose configuration. TM
%   Sthlm 1802:      define directories:  tornado m file, data home, airfoil
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
persistent perssettings % store directory names; set by config('startup',xx,yy,zz),
% read by config('getdir');
output = [];
switch input
    case 'startup'
        switch nargin
            case 1   % old organization; assumes we start in TORNADO mfile home, 
                output.hdir=pwd;
                output.acdir=strcat(output.hdir ,filesep,'aircraft');   %aircraft directory
                output.afdir=strcat(output.acdir,filesep,'airfoil' );   %airfoil directory
                output.sdir =strcat(output.hdir ,filesep,'state'   );   %state directory
                output.odir =strcat(output.hdir ,filesep,'output'  );   %output file directory
                perssettings = output;

            case 2  % old organization: thomedir is tornado mfile home; 
                    % same as data home directory; comes with aircraft etc.
                output.hdir = thomedir;
                output.acdir=strcat(output.hdir ,filesep,'aircraft');   %aircraft directory
                output.afdir=strcat(output.acdir,filesep,'airfoil' );   %airfoil directory
                output.sdir =strcat(output.hdir ,filesep,'state'   );   %state directory
                output.odir =strcat(output.hdir ,filesep,'output'  );   %output file directory
                perssettings = output;

            case 3  % new org.: thomedir is tornado mfile home; includes airfoil library, 
                    % datahomedir becomes host to aircraft, state, output
                output.hdir = thomedir;
                output.afdir=strcat(thomedir,filesep,'airfoil' );       %airfoil directory
                % datahomedir is data home directory
                output.acdir=strcat(datahomedir ,filesep,'aircraft');   %aircraft directory
                output.sdir =strcat(datahomedir ,filesep,'state'   );   %state directory
                output.odir =strcat(datahomedir ,filesep,'output'  );   %output file directory
                % if first call, must create the directories
                try
                    cd(datahomedir)
                    % create aircraft, use existing if exists
                    [succa,mess,mid ] = mkdir('aircraft');
                    [succs,mess,mid ] = mkdir('state'   );
                    [succo,mess,mid ] = mkdir('output'  );
                    if succa && succs && succo
                        % all is well
                        perssettings = output;
                        return
                    else
                        output = [];
                    end
                catch
                    disp(['project directory ',datahomedir,' must exist, but does not'])
                    output = [];
                    return
                end
                % JO 160511
                perssettings = output;
                case 4  % thomedir is tornado mfile home; datahomedir is .. that
                        % afdir is air foil directory
                output.hdir = thomedir;
                output.afdir= afdir;       %airfoil directory
                % datahomedir is data home directory
                output.acdir=strcat(datahomedir ,filesep,'aircraft');   %aircraft directory
                output.sdir =strcat(datahomedir ,filesep,'state'   );   %state directory
                output.odir =strcat(datahomedir ,filesep,'output'  );   %output file directory
                % if first call, must create the directories
                try
                    cd(datahomedir)
                    % create aircraft, use existing if exists
                    [succa,mess,mid ] = mkdir('aircraft');
                    [succs,mess,mid ] = mkdir('state'   );
                    [succo,mess,mid ] = mkdir('output'  );
                    if succa && succs && succo
                        % all is well
                        perssettings = output;
                        return
                    else
                        output = [];
                    end
                catch
                    disp(['project directory ',datahomedir,' must exist, but does not'])
                    output = [];
                    return
                end
                % JO 160511
                perssettings = output;

            otherwise
                disp(' too many arguments to config');
                output = [];
                return
        end

        % JOp 090614
    case 'getdir'
        output = perssettings;

    case 'S_ref'
        output=[];		%[m^2]	reference surface

    case 'b_ref'
        output=[];		%[m] reference span

    case 'C_mac'
        output=[];		%[m] mean aerodynamic chord, reference chord

    case 'mac_pos'
        output=[];   %[m m m] , xyz triplet, start of C_mac

    case 'infinity'
        output=[];	%Arbitrary length of wake constant

    case 'near'
        output=1e-3; %Vortex core radius, change if solutions are in bad condition

    case 'verbose'
        output=0;   %Set to 1 for talkative output, 0 for silent

    case 'delta'
        output=0.001;  %Delta change in varables during finite difference

    otherwise
        output=0;
end
