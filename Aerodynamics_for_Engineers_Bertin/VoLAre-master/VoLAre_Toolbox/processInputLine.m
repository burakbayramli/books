% Function to process input file line
% Anthony Ricciardi
%
% Inputs
% A = [string] line from input file
% continuation = [dummy] input used to identify that line is a continuation 
%                        line.  Additional checks are performed.  
%
% Outputs
% B = Cell structured input line
%
function B = processInputLine(A,continuation)

% fid = fopen('model_N.bdf','r');
% % fseek(fid,-500,'eof')
% A = fgetl(fid);

%% Checks
if nargin > 1
    if A == -1
        error('The file has ended where there should be a continuation entry')
    else
        Acheck = strrep(A,' ','');
        if strcmp(Acheck,'') == 1
            error('Don''t put blank lines in between intial lines and continuation entries.')
        elseif strcmp(Acheck(1),'$') == 1
            error('Don''t put comment lines in between intial lines and continuation entries.')
        elseif isempty(strfind(A,'*'))~=1
            error('Large Field Format not supported')
        end
    end
end

commas=strfind(A,',');
if isempty(commas) ~= 1
    %% Free Field Format - Input data fields are separated by commas.
    B = textscan(A,'%s','Delimiter',',','CommentStyle','$');
    B = B{1}';
    sb = size(B,2);
    if sb < 9
        B2 = {'','','','','','','','',''};
        B2(1:sb) = B;
        B = B2;
    end 
else
%% Small Field Format - 10 fields of eight characters each (9 are used). 
    sa = size(A,2);
%     if sa > 80
%         error(['Line too long: ',A,])
    if sa >= 73
        B = {A(1:8),A(9:16),A(17:24),A(25:32),A(33:40),A(41:48),A(49:56),A(57:64),A(65:72)};
    elseif sa >= 65
        B = {A(1:8),A(9:16),A(17:24),A(25:32),A(33:40),A(41:48),A(49:56),A(57:64),A(65:end)};
    elseif sa >= 57
        B = {A(1:8),A(9:16),A(17:24),A(25:32),A(33:40),A(41:48),A(49:56),A(57:end),''};
    elseif sa >= 49
        B = {A(1:8),A(9:16),A(17:24),A(25:32),A(33:40),A(41:48),A(49:end),'',''};
    elseif sa >= 41
        B = {A(1:8),A(9:16),A(17:24),A(25:32),A(33:40),A(41:end),'','',''};
    elseif sa >= 32
        B = {A(1:8),A(9:16),A(17:24),A(25:32),A(33:end),'','','',''};
    elseif sa >= 25
        B = {A(1:8),A(9:16),A(17:24),A(25:end),'','','','',''};
    elseif sa >= 17
        B = {A(1:8),A(9:16),A(17:end),'','','','','',''};
    elseif sa >= 9
        B = {A(1:8),A(9:end),'','','','','','',''};
    else
        B = {A(1:end),'','','','','','','',''};
    end
end
B = strrep(B,' ',''); % remove spaces
