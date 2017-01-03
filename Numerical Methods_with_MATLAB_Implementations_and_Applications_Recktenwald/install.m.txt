% Installation instructions for The Numerical Methods with MATLAB Toolbox
%
% See also:    Readme.m
%              www.me.pdx.edu/~gerry/nmm
%              www.prenhall.com/recktenwald
% 
% Installation:
% ------------
%
%   1.  Download the complete NMM archive in a form compatible with
%       your computer from
%
%             www.prenhall.com/recktenwald
%       or
%             www.me.pdx.edu/~gerry/nmm
%
%   2.  Unpack the archive and copy its entire contents to a suitable
%       place on the hard drive of your computer.  The definition of
%       "suitable place" depends on whether or not you can install MATLAB,
%       and copy files into the MATLAB home directory on your computer.
%       If you are running MATLAB from a server (e.g. a unix host or
%       PC-based file server) then you should consult your system administrator
%       or follow step 4., below.  If you are running MATLAB on a
%       personal computer *and* MATLAB is installed on that computer,
%       *and* you can copy files into the MATLAB home directory, then
%       follow the instructions in step 3 below.
%
%   3.  For users with access to the MATLAB home directory
%
%         a.  Create a directory called "nmm" in the MATLAB Toolbox directory
%         b.  Unpack the archive and copy all the directories and files to
%             the newly created nmm directory.  Keep the directories in the
%             archive intact!  There is no need (unless you are organizationally
%             challenged or obsession averse) to put all the files into
%             one nmm directory.
%         c.  Start MATLAB (or restart it if it was running while you installed
%             the nmm toolbox files)
%         d.  Using the MATLAB path browser, add the nmm directory and all of
%             its subdirectories to the default path.  Consult the documentation
%             that came with MATLAB on how to use the path browser.
%         e.  Verify the installation by enterring the following commands at the
%             command prompt
%
%                 >> help nmm
%                 >> info nmm
%                 >> nmmCheck(0)
%
%             The response to the "help nmm" command should be a list of all
%             nmm toolbox files.  The response to the "info nmm" command
%             should be a listing of the readme.m file.  The response to
%             "nmmCheck(0)" should be a summary print out of the number and
%             types of files in the NMM toolbox.
%
%
%   4.  For users without access to the MATLAB home directory, show the following
%       instructions to your system administrator.  Once the system administrator
%       sets up the network version of the NMM Toolbox, all of the toolbox
%       files will be automatically available to all users.
%
%       Set-up instructions for the system administrator:
%
%         a.  Create a directory called "nmm" in the MATLAB Toolbox directory
%         b.  Unpack the archive and copy all the directories and files to
%             the newly created nmm directory.  Keep the directories in the
%             archive intact!
%         c.  Edit the pathdef.m file by adding the contents of the nmmPathDef.m
%             file in the root directory of the NMM archive.  The pathdef.m file
%             is in the MATLABHOME/toolbox/local directory of the MATLAB installation.
%             The MATLABHOME directory is the root directory of the installation
%         d.  Log on as a generic user, launch MATLAB, and run
%
%                 >> help nmm
%                 >> info nmm
%                 >> nmmCheck(0)
%
%             The response to the "help nmm" command should be a list of all
%             nmm toolbox files.  The response to the "info nmm" command
%             should be a listing of the readme.m file.  The response to
%             "nmmCheck(0)" should be a summary print out of the number and
%             types of files in the NMM toolbox.
%
%         e.  Tell network users how to configure their account according to local
%             customs:
%
%               (i)  Unix users may want to create a MATLAB subdirectory and
%                    add shell scripts so that MATLAB starts up in that directory
%               (ii) Windows users may want to add MATLAB to the start menu
%                    and/or create an icon on the desktop.  In either case the
%                    ``start in'' directory of the icon properties should probably
%                    be a user directory containing handy configuration scripts.
%
%            ---------------------------------
