%% Read Model Keyword/Input Data File
%Author: Michael Okereke
%Date:   7th August, 2014
%About:  This script reads the model Keyword file. The data is subsequently
%        passed onto the Solution Engine of the MATFESolver.

%% -----------------------------------------------------------------------
%% Clear Workspace and Command Window
    clc; clear;
%% -----------------------------------------------------------------------
%% Read contents of the data file
for ReadComputeDATAFile = 1:1
    % Search for *.dat files .  In this mode, ALL *.dat files must
    % be in the same directory.
        directory = pwd; filesep = '\';
        filenames = {[directory, filesep, '*.dat']};
        file = dir( char(filenames) );
    %Copy *.DAT file content and output into a temporary *.m file for
    %evaluation of the contents
        for r=1:length(file)      
            if( exist( file.name, 'file' ) == 2 )
                %Create a temporary script file from the dat file
                copyfile(file.name,sprintf('tempDATMFileFor%s.m',mat2str(r)));
                %Run/Evaluate the data script file
                eval(sprintf('tempDATMFileFor%s',mat2str(r)));
            end
        end
        %Delete file after evaluating its contents
        delete('tempDATMFileFor1.m')
end