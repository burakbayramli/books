% Read free field or small field NATRAN input file and convert to cell structure data model
% Anthony Ricciardi
%
% Inputs
% filename = [string] text file input in ZAERO format
%
% Output
% FEM [cell] input file data
%
function FEM = importFieldModelAdvanced(filename)

fid = fopen(filename);

%% Entries
entries.name(1) = {'CAERO7'};
entries.name(2) = {'PAFOIL7'};
entries.name(3) = {'AEFACT'};
entries.name(4) = {'ACOORD'};
entries.name(5) = {'AEROZ'};
entries.name(6) = {'AESURFZ'};
entries.name(7) = {'PANLST2'};
entries.name(8) = {'CORD2R'};
entries.name(9) = {'TRIM'};
entries.name(10)= {'TRIMVAR'};
entries.name(11)= {'TRIMFLT'};
entries.name(12)= {'SPLINE1'};
entries.name(13)= {'SET1'};
entries.name(14)= {'PANLST1'};
entries.name(15)= {'SPLINEF'};
entries.name(16)= {'PANLST3'};


%% check and intialize counters
% size(entries.name) == size(entries.data) == size(entries.minRows) == size(entries.maxRows)
for i = 1:size(entries.name,2)
    eval(strcat('entries.Num(',num2str(i),')=0;'))
end

%% Process input
C = nextLine(fid);
if iscell(C) == 0
    if C == -1
        error('The input file is empty')
    end
end
one = C{1};

placeHolder = [];
while 1
    % New Entry
    ety = [];
    for i = 1:size(entries.name,2)
        if strcmp(one,entries.name{i})
            ety = i;
            break %%%%%% EXIT  FOR %%%%%%%
        end
    end
    if isempty(ety); error(strcat(one,' entry not supported.')); end
    
    row = 1; % continuation line
    eval(strcat('entries.Num(',num2str(i),') = entries.Num(',num2str(i),') + 1;')) % advance counter
        
    while 1
        eval(['placeHolder = f',entries.name{ety},'(''readline'',row,placeHolder,C);']); % placeHolder = fcaero7('readline',row,placeHolder,C);
        
        % read next line
        C = nextLine(fid);
        if iscell(C) == 0
            % FEM.CAERO7(entries.Num(ety)) = placeHolder;
            eval(['FEM.',entries.name{ety},'(',num2str(entries.Num(ety)),')','= placeHolder;']);
            return %%%%%%%%%%%%%%%%%%%% EXIT  WHILE and FUNCTION %%%%%%%%%%
        end
        one = C{1};
        
        % check continuation
        if ischar(one)
            oneCheck = strrep(one,' ','');
            if strcmp(oneCheck,'') == 1
                one = [];
            elseif oneCheck(1) == '+';
                one = [];
            end
        end
               
        if isempty(one)
            % continuation line
            row = row + 1;
            if row > eval(['f',entries.name{ety},'(''maxRows'')'])
                error(['Max rows excedded in ',entries.name{ety},' entry'])
            end
        else
            % next entry
            eval(['FEM.',entries.name{ety},'(',num2str(entries.Num(ety)),')','= placeHolder;']);
            placeHolder = [];
            if row < eval(['f',entries.name{ety},'(''minRows'')'])
                error(['Required continuations missing in ',entries.name{ety},' entry'])
            end
            break %%%%%%%%%%%%%%%%%%%% EXIT  WHILE  endEntry %%%%%%%%%%%%%%
        end
    end
end
end
% fclose('all');

function C = nextLine(fid)
C = [];

A = fgetl(fid);
if A == -1
    % break % The file has ended
    C = -1;
else
    Acheck = strrep(A,' ','');
    
    if strcmp(Acheck,'') == 1
        % Empty line will be skipped
    elseif strcmp(Acheck(1),'$') == 1
        % Comment line will be skipped
    elseif isempty(strfind(A,'*'))~=1
        error('Large field format not supported')
    else
        % read line
        C = processInputLine(A);
    end
end

if isempty(C)
    C = nextLine(fid);
end

end