function mps2mat(filename)
% Filename: mps2mat.m
% Description: Converts an MPS file to a MAT file
% Authors: Ploskas, N., & Samaras, N., Triantafyllidis, Ch.
%
% Syntax: mps2mat(filename)
%
% Input:
% -- filename: the name of the MPS file (without the .mps
%    extension)
%
% Output: a MAT file named filename.mat

% set the search starting point to 1
searchStartingPoint = 1;
% create an empty vector for the objective constant
c0 = [];
% add the extension to the mps file
mpsFilename = [filename '.mps'];
% find a rough estimation for the number of variables and
% the nonzero elements
s = dir(mpsFilename);
filesize = s.bytes;
estimation = floor(filesize / 220);
nnzmax = floor((filesize / 30));
fid = fopen(mpsFilename, 'rt'); % open the file
Eqin = []; % create an empty vector for Eqin
MinMaxLP = []; % create an empty vector for MinMaxLP
% find the name of the problem and the optimization
% direction
line = fgets(fid);
% read the NAME header
useless = sscanf(line(1, 1:4), '%s%', 1);
% if the optimization direction is included
if length(line) > 23
    % read the Name
    Name = sscanf(line(1, 15:23), '%s%', 1);
    % read the optimization direction
    MinMaxLP = sscanf(line(1, 75:length(line)), '%s%', 1);
else % if the optimization direction is not included
    % read the name
    Name = sscanf(line(1, 15:length(line)), '%s%', 1);
    MinMaxLP = []; % optimization direction does not exist
end
% if the optimization direction is not empty
if isempty(MinMaxLP) ~= 1
    if isequal(MinMaxLP, 'MIN') == 1 % minimization
        MinMaxLP = -1;
    elseif isequal(MinMaxLP, 'MAX') == 1 % maximization
        MinMaxLP = 1;
    end
else % if it is empty, the default is minimization
    MinMaxLP = -1;
end
fgetl(fid);
constraintType = '';
objPos = 1;
k = 1;
lineE = 1;
% read the ROWS section and populate vector Eqin
while isequal(constraintType, 'COLUMNS') == 0
    % get the name and type of the constraints
    constraintType = fscanf(fid, '%s%', 1);
    if isequal(constraintType, 'COLUMNS') ~= 1
        constraintName = fscanf(fid, '%s%', 1);
    else % break when you read the COLUMNS header
        break
    end
    X{k, 1} = constraintType;
    X{k, 2} = constraintName;
    if isequal(X{k, 1}, 'N') == 1 % objective function
        objPos = k;
        ant = X{k, 2};
    % equality constraint
    elseif isequal(constraintType, 'E' ) == 1
        Eqin(lineE) = 0;
        lineE = lineE + 1;
    % less than or equal to constraint
    elseif isequal(constraintType, 'L') == 1
        Eqin(lineE) = -1;
        lineE = lineE + 1;
    % greater than or equal to constraint
    elseif isequal(constraintType, 'G') == 1
        Eqin(lineE) = 1;
        lineE = lineE + 1;
    end
    k = k + 1;
end
Eqin = Eqin'; % transpose Eqin
% read COLUMNS section and populate vector c and matrix A
X(objPos, :) = [];
[m, ~] = size(X);
A = spalloc(m, estimation, nnzmax); % preallocate matrix A
B = zeros(m, 1); %create vector B
C = {}; % create cell C
R = []; % create vector R
BS = {}; % create vector BS
clmn = 1;
variables = {};
objectivity = 0;
fgets(fid);
D = X(:, 2);
flag = 0;
[a, ~] = size(D);
D{a + 1, :} = ant;
D = char(D);
mas = length(D);
objInd = a + 1;
% read the first line in COLUMNS header
line = fgets(fid);
b = length(line);
% read the variable name, the constraint name and the value
varname = sscanf(line(1, 5:12), '%s%', 1);
varpos = sscanf(line(1, 15:22), '%s%', 1);
value = sscanf(line(1, 25:b), '%s%', 1);
value = str2double(value);
if b > 40 % read the fifth and the sixth fields if exist
    flag = 1;
    varpos2 = sscanf(line(1, 40:b), '%s%', 1);
    value2 = str2double(line(1, 50:b));
end
% check if during the reading we changed the current variable
if objectivity == 0
    C{clmn} = 0;
else
    objectivity = 0;
end
currentvar = varname;
clmn = clmn + 1;
variables{clmn, 1} = currentvar;
k = strmatch(varpos, D, 'exact');
% store the value to vector C if the variable refers to the
% objective function
if k == objInd
    C{clmn} = value;
    objectivity = 1;
else
    A(k, clmn) = value; % store to matrix A
end
if b > 40 % if exists the fifth and sixth fields
    k2 = strmatch(varpos2, D, 'exact');
    % store the value to vector C if the variable refers to
    % the objective function
    if k2 == objInd
        C{clmn} = value2;
        objectivity = 1;
    else
        A(k2, clmn) = value2; % store to matrix A
    end
end
% read the second line in COLUMNS header
varname = '';
varpos = '';
value = '';
varpos2 = '';
value2 = '';
line = fgets(fid);
b = length(line);
% if we reached the RHS section
if isequal(line(1, 1:3), 'RHS')
    if objectivity == 0
        C{clmn} = 0;
    end
end
% read the variable name, the constraint name and the value
varname = sscanf(line(1, 5:12), '%s%', 1);
varpos = sscanf(line(1, 15:22), '%s%', 1);
value = sscanf(line(1, 25:b), '%s%', 1);
value = str2double(value);
if b > 40 % read the fifth and the sixth fields if exist
    varpos2 = sscanf(line(1, 40:48), '%s%', 1);
    value2 = str2double(line(1, 50:b));
    flag = 1;
end
% if the variable changes, then we must reset the starting
% point of the search
if isequal(varname, currentvar) == 0
    searchStartingPoint = 1;
    % check if during the reading we changed the current
    % variable
    if objectivity == 0
        C{clmn} = 0;
    else
        objectivity = 0;
    end
    currentvar = varname;
    clmn = clmn + 1;
    variables{clmn, 1} = currentvar;
end
k = strmatch(varpos, D(searchStartingPoint:mas, :), ...
    'exact');
if searchStartingPoint ~= 1
    k = k + searchStartingPoint - 1;
end
if isempty(k)
    k = strmatch(varpos, D(1:searchStartingPoint - 1, ...
        :),'exact');
end
searchStartingPoint = k + 1;
% store the value to vector C if the variable refers to
% the objective function
if k == objInd
    C{clmn} = value;
    objectivity = 1;
else
    % store to matrix A
    A(k, clmn) = value;
end
if b > 40 % if exists the fifth and sixth fields
    k2 = strmatch(varpos2, D(searchStartingPoint:mas, ...
        :), 'exact');
    if searchStartingPoint ~= 1
        k2 = k2 + searchStartingPoint - 1;
    end
    if isempty(k2)
        k2 = strmatch(varpos2, D(1:searchStartingPoint ...
            - 1, :), ...
            'exact');
    end
    searchStartingPoint = k2 + 1;
    % store the value to vector C if the variable refers to
    % the objective function
    if k2 == objInd
        C{clmn} = value2;
        objectivity = 1;
    else
        A(k2, clmn) = value2; % store to matrix A
    end
end
% read the rest of the records of COLUMNS section
% set the first found index to use later for faster search
searchStartingPoint = k + 1;
flag = 1;
if flag == 1
    % stop when you reach the RHS section
    while isequal(varname, 'RHS') == 0
        varname = '';
        varpos = '';
        value = '';
        varpos2 = '';
        value2 = '';
        line = fgets(fid);
        b = length(line);
        % if we reach the RHS section, then break the loop
        if isequal(line(1, 1:3),'RHS')
            if objectivity == 0
                C{clmn} = 0;
            end
            break
        end
        % read the variable name, the constraint name and
        % the value
        varname = sscanf(line(1, 5:12), '%s%', 1);
        varpos = sscanf(line(1, 15:22), '%s%', 1);
        value = sscanf(line(1, 25:b), '%s%', 1);
        value = str2double(value);
        % read the fifth and the sixth fields if exist
        if b > 40
            varpos2 = sscanf(line(1, 40:b), '%s%', 1);
            value2 = str2double(line(1, 50:b));
        end
        if isequal(varname, currentvar) == 0
            % if the variable changes, then we must reset
            % the starting point of the search
            searchStartingPoint = 1;
            % check if during the reading we changed the
            % current variable
            if objectivity == 0
                C{clmn} = 0;
            else
                objectivity = 0;
            end
            currentvar = varname;
            clmn = clmn + 1;
            variables{clmn, 1} = currentvar;
        end
        k = strmatch(varpos, D(searchStartingPoint:mas, ...
            :), 'exact');
        if searchStartingPoint ~= 1
            k = k + searchStartingPoint - 1;
        end
        if isempty(k)
            k = strmatch(varpos, D(1:searchStartingPoint ...
                - 1, :), 'exact');
        end
        searchStartingPoint = k + 1;
        % store the value to vector C if the variable refers
        % to the objective function
        if k == objInd
            C{clmn} = value;
            objectivity = 1;
        else
            % store to matrix A
            A(k, clmn) = value;
        end
        if b > 40 % if exists the fifth and sixth fields
            k2 = strmatch(varpos2, ...
                D(searchStartingPoint:mas, :), 'exact');
            if searchStartingPoint ~= 1
                k2 = k2 + searchStartingPoint - 1;
            end
            if isempty(k2)
                k2 = strmatch(varpos2, ...
                    D(1:searchStartingPoint - 1, :), ...
                    'exact');
            end
            searchStartingPoint = k2 + 1;
            % store the value to vector C if the variable
            % refers to the objective function
            if k2 == objInd
                C{clmn} = value2;
                objectivity = 1;
            else
                A(k2, clmn) = value2; % store to matrix A
            end
        end
    end
end
% process the final record of COLUMNS section
if flag == 0
    % stop when the RHS section is reached
    while isequal(varname, 'RHS') == 0
        varname = '';
        varpos = '';
        value = '';
        line = fgets(fid);
        b = length(line);
        if isequal(line(1, 1:3), 'RHS')
            if objectivity == 0
                C{clmn} = 0;
            end
            break
        end
        % read the variable name, the constraint name and
        % the value
        varname = sscanf(line(1, 5:12), '%s%', 1);
        varpos = sscanf(line(1, 15:22), '%s%', 1);
        value = sscanf(line(1, 25:b), '%s%', 1);
        value = str2double(value);
        if isequal(varname, currentvar) == 0
            % if the variable changes, then we must reset
            % the starting point of the search
            searchStartingPoint = 1;
            % check if during the reading we changed the
            % current variable
            if objectivity == 0
                C{clmn} = 0;
            else
                objectivity = 0;
            end
            currentvar = varname;
            clmn = clmn + 1;
            variables{clmn, 1} = currentvar;
        end
        k = strmatch(varpos, ...
            D(searchStartingPoint:mas, :), 'exact');
        if searchStartingPoint ~= 1
            k = k + searchStartingPoint - 1;
        end
        if isempty(k)
            k = strmatch(varpos, ...
                D(1:searchStartingPoint - 1, :), 'exact');
        end
        searchStartingPoint = k + 1;
        % store the value to vector C if the variable
        % refers to the objective function
        if k == objInd
            C{clmn} = value;
            objectivity = 1;
        else
            A(k, clmn) = value; % store to matrix A
        end
    end
end
A(:, 1) = [];
variables(1, :) = [];
[x, ~] = size(variables);
% checking for any mistakes made during the preallocation
% of matrix A
if estimation > x
    A(:, x + 1:(estimation - 1)) = [];
end
C(:, 1) = [];
[fr, ~] = size(A);
rflag = 0;
bflag = 0;
% read the RHS section and populate vector b
% stop if the end of the MPS file reached
while isequal(varname, 'ENDATA') == 0
    varname = '';
    varpos = '';
    value = '';
    varpos2 = '';
    value2 = '';
    line = fgets(fid);
    b = length(line);
    % stop if the end of the MPS file reached
    if isequal(line(1, 1:6), 'ENDATA')
        rflag = 0;
        bflag = 0;
        break
        % stop if we reached the RANGES section
    elseif isequal(line(1, 1:6), 'RANGES')
        rflag = 1;
        break
        % stop if we reached the BOUNDS section
    elseif isequal(line(1, 1:6), 'BOUNDS')
        bflag = 1;
        break
    end
    % read the right-hand side name, the constraint name
    % and the value
    varname = sscanf(line(1, 5:12), '%s%', 1);
    varpos = sscanf(line(1, 15:22), '%s%', 1);
    value = sscanf(line(1, 25:b), '%s%', 1);
    value = str2double(value);
    if b > 40 % if exists the fifth and sixth fields
        varpos2 = sscanf(line(1, 40:b), '%s%', 1);
        value2 = str2double(line(1, 50:b));
        k2 = strmatch(varpos2, D, 'exact');
        B(k2) = value2; % store the value to vector B
    end
    k = strmatch(varpos, D, 'exact');
    B(k) = value; % store the value to vector B
end
frb = length(B);
if frb > fr % check if the objective has a constant
    B(frb) = [];
end
% read the RANGES section and populate matrix R
if rflag == 1
    range_ind = 1;
    % stop if the end of the MPS file reached
    while isequal(varname, 'ENDATA') == 0
        varname = '';
        varpos = '';
        value = '';
        varpos2 = '';
        value2 = '';
        line = fgets(fid);
        b = length(line);
        % stop if the end of the MPS file reached
        if isequal(line(1, 1:6), 'ENDATA')
            bflag = 0;
            break
            % stop if we reached the BOUNDS section
        elseif isequal(line(1, 1:6), 'BOUNDS')
            bflag = 1;
            break
        end
        % read the range name, the constraint name and
        % the value
        varname = sscanf(line(1, 5:12), '%s%', 1);
        varpos = sscanf(line(1, 15:22), '%s%', 1);
        value = sscanf(line(1, 25:b), '%s%', 1);
        value = str2double(value);
        if b > 40 % if exists the fifth and sixth fields
            varpos2 = sscanf(line(1, 40:b), '%s%', 1);
            value2 = str2double(line(1, 50:b));
        end
        k = strmatch(varpos, D, 'exact');
        R(range_ind, 1) = k;
        % store range to matrix R
        if isequal(X{k, 1}, 'E')
            if value > 0 % Type of row E and sign of the
                % range value +
                R(range_ind, 2) = B(k);
                R(range_ind, 3) = (B(k) + abs(value));
                R(range_ind, 4) = 1;
            elseif value < 0 % Type of row E and sign of
                % the range value -
                R(range_ind, 2) = B(k) - abs(value);
                R(range_ind, 3) = B(k);
                R(range_ind, 4) = -1;
            end
        elseif isequal(X{k, 1}, 'L') % Type of row L
            R(range_ind, 2) = B(k) - abs(value);
            R(range_ind, 3) = B(k);
            if value > 0
                R(range_ind, 4) = 1;
            else
                R(range_ind, 4) = -1;
            end
        elseif isequal(X{k, 1}, 'G') % Type of row G
            R(range_ind, 2) = B(k);
            R(range_ind, 3) = B(k) + abs(value);
            if value > 0
                R(range_ind, 4) = 1;
            else
                R(range_ind, 4) = -1;
            end
        end
        range_ind = range_ind + 1;
        k = strmatch(varpos2, D, 'exact');
        if isempty(k) ~= 1
            R(range_ind, 1) = k;
            % store range to matrix R
            if isequal(X{k, 1}, 'E')
                if value2 > 0 % Type of row E and sign of
                    % the range value +
                    R(range_ind, 2) = B(k);
                    R(range_ind, 3) = B(k) + abs(value2);
                    R(range_ind, 4) = 1;
                elseif value2 < 0 % Type of row E and sign
                    % of the range value -
                    R(range_ind, 2) = B(k) - abs(value2);
                    R(range_ind, 3) = B(k);
                    R(range_ind, 4) = -1;
                end
            elseif isequal(X{k, 1}, 'L') % Type of row L
                R(range_ind, 2) = B(k) - abs(value2);
                R(range_ind, 3) = B(k);
                if value2 > 0
                    R(range_ind, 4) = 1;
                else
                    R(range_ind, 4) = -1;
                end
            elseif isequal(X{k, 1}, 'G') % Type of row G
                R(range_ind, 2) = B(k);
                R(range_ind, 3) = B(k) + abs(value2);
                if value2 > 0
                    R(range_ind, 4) = 1;
                else
                    R(range_ind, 4) = -1;
                end
            end
            range_ind = range_ind + 1;
        end
    end
end
% read the BOUNDS section and populate matrix BS
if bflag == 1
    D = variables(:, 1);
    % possible bound types
    Types = {'LO' 'UP' 'FX' 'FR' 'MI' 'PL'};
    bound_ind = 1;
    while isequal(varname, 'ENDATA') == 0
        boundtype = '';
        varname = '';
        value = '';
        line = fgets(fid);
        b = length(line);
        % stop if the end of the MPS file reached
        if isequal(line(1, 1:6), 'ENDATA')
            break
        end
        % read the bound type, the bound name, the
        % variable name and the value
        boundtype = sscanf(line(1, 2:4), '%s%', 1);
        if b > 22 % % LO, UP and FX type of constraints
            % have a value field
            varname = sscanf(line(1, 15:22), '%s%', 1);
            value = sscanf(line(1, 25:b), '%s%', 1);
            value = str2double(value);
            BS{bound_ind, 3} = value;
        else % FR, MI, PL type of constraints do not
            % have a value field
            varname = sscanf(line(1, 15:b), '%s%', 1);
            BS{bound_ind, 3} = NaN;
        end
        k = strmatch(varname, D, 'exact');
        BS{bound_ind, 1} = k;
        k2 = strmatch(boundtype, Types, 'exact');
        if k2 == 1 % LO bound
            BS{bound_ind, 2} = 1;
        elseif k2 == 2 % UP bound
            BS{bound_ind, 2} = -1;
        elseif k2 == 3 % FX bound
            BS{bound_ind, 2} = 0;
        elseif k2 == 4 % FR bound
            BS{bound_ind, 2} = 11;
        elseif k2 == 5 % MI bound
            BS{bound_ind, 2} = 111;
        elseif k2 == 6 % MV bound
            BS{bound_ind, 2} = 1111;
        end
        bound_ind = bound_ind + 1;
    end
end
c = cell2mat(C); % convert cell to matrix
c = sparse(c'); % transpose and make vector c sparse
% calculate the nonzero elements
NonZeros = nnz(A);
b = sparse(B); % make vector b sparse
A = sparse(A); % make matrix A sparse
R = sparse(R); % make matrix R sparse
BS = cell2mat(BS); % convert cell to mat
BS = sparse(BS); % make matrix BS sparse
Eqin = sparse(Eqin); % make vector Eqin sparse
Name = filename;
% save to file
c0 = 0;
save matlab.mat Name c A b R BS Eqin NonZeros ...
    MinMaxLP c0
copyfile('matlab.mat', [filename '.mat']); % copy file
fclose(fid); % close the file
delete matlab.mat % delete matlab.mat
end