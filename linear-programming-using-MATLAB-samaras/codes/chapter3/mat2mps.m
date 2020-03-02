function mat2mps(filename)
% Filename: mat2mps.m
% Description: Converts a MAT file to an MPS file
% Authors: Ploskas, N., & Samaras, N., Triantafyllidis, Ch.
%
% Syntax: mat2mps(filename)
%
% Input:
% -- filename: the name of the MAT file (without the .mat
%    extension)
%
% Output: an MPS file named filename.mps

% copy the specified file to matlab.mat
copyfile([filename '.mat'], 'matlab.mat');
load; % load file
A = full(A); % make A full
b = full(b); % make b full
c = full(c); % make c full
Eqin = full(Eqin); % make Eqin full
C = c; % copy c to C
B = b; % copy b to B
% if MinMaxLP does not exist, the LP problem is minimization
if exist('MinMaxLP', 'var') == 0
    MinMaxLP = -1;
end
% if the Name does not exist, use the filename
if exist('Name', 'var') == 0
    Name = filename;
end
% if R does not exist, set to empty
if exist('R', 'var') == 0
    R = [];
else
    R = full(R);
end
% if BS does not exist, set to empty
if exist('BS','var') == 0
    BS = [];
else
    BS = full(BS);
end
% create an mps file with the same name of the LP and write
% MPS filename to it
y = length(Name);
Name2 = Name;
Name(1, y + 1) = '.';
Name(1, y + 2) = 'm';
Name(1, y + 3) = 'p';
Name(1, y + 4) = 's';
fid = fopen(Name, 'wt');
id = ['NAME          ',Name2];
fprintf(fid, '%s', id);
for i = 1:50 + (8 - y)
    nullname(i) = ' ';
end
% write the type of optimization to the MPS file
if MinMaxLP == -1 % minimization
    fprintf(fid, '%s', nullname);
    fprintf(fid, '%s', '( MIN)');
elseif MinMaxLP == 1 % maximization
    fprintf(fid, '%s', nullname);
    fprintf(fid, '%s', '( MAX)');
end
% create ROWS section
fprintf(fid, '\n');
fprintf(fid, '%s', 'ROWS'); % print the ROWS header
fprintf(fid, '\n');
obj = [' N  OBJ'];
fprintf(fid, '%s', obj); % print the objective function
fprintf(fid, '\n');
a = length(Eqin);
Eqin2 = {};
for i = 1:a
    if Eqin(i) == -1 % less than or equal to constraint
        Eqin2{i} = 'L';
    elseif Eqin(i) == 0 % equality constraint
        Eqin2{i} = 'E';
    elseif Eqin(i) == 1 % greater than or equal to constraint
        Eqin2{i} = 'G';
    end
end
Eqin2 = cell2mat(Eqin2); % convert cell to matrix
for i = 1:a % print constraints
    fprintf(fid, '%s', ' ');
    fprintf(fid, '%s', Eqin2(i));
    fprintf(fid, '%s', '  ');
    fprintf(fid, '%s', 'R');
    fprintf(fid, '%i', i);
    fprintf(fid, '\n');
end
% create COLUMNS section
fprintf(fid, '%s', 'COLUMNS'); % print COLUMNS header
[a, b] = size(A);
fprintf(fid, '\n');
for i = 1:b % column-oriented
    if i > 1
        if C(i - 1) ~= 0 % new line after each record
            fprintf(fid, '\n');
        end
    end
    j = 1;
    while j <= a
        if A(j, i) ~= 0 % print the constraint matrix A
            fprintf(fid, '%s', '    X');
            fprintf(fid, '%i', i);
            szcount = num2str(i);
            y = length(szcount);
            k = 5 - y;
            for m = 1:k
                fprintf(fid, '%s', ' ');
            end
            fprintf(fid, '%s', '    ');
            fprintf(fid, '%s', 'R');
            fprintf(fid, '%i', j);
            szcount = num2str(j);
            y = length(szcount);
            k = 5 - y;
            for m = 1:k
                fprintf(fid, '%s', ' ');
            end
            fprintf(fid, '%s', '    ');
            fprintf(fid, '%f', A(j, i));
            fprintf(fid, '\n');
        end
        if j < a
            j = j + 1;
        else
            if C(i) ~= 0 % print the objective function
                fprintf(fid, '%s', '    X');
                fprintf(fid, '%i', i);
                szcount = num2str(i);
                y = length(szcount);
                k = 5 - y;
                for m = 1:k
                    fprintf(fid, '%s', ' ');
                end
                fprintf(fid, '%s', '    ');
                fprintf(fid, '%s', 'OBJ');
                fprintf(fid, '%s', '       ');
                fprintf(fid, '%f', C(i));
                if i == b
                    if j == a
                        fprintf(fid, '\n');
                    end
                end
            end
            break
        end
    end
end
% create RHS section
fprintf(fid, '%s', 'RHS'); % print the RHS header
fprintf(fid, '\n');
a = length(B);
for i = 1:a % print each right-hand side
    fprintf(fid, '%s', '    RHS1');
    fprintf(fid, '%s', '      ');
    fprintf(fid, '%s', 'R');
    fprintf(fid, '%i', i);
    szcount = num2str(i);
    y = length(szcount);
    k = 5 - y;
    for m = 1:k
        fprintf(fid, '%s', ' ');
    end
    fprintf(fid, '%s', '    ');
    fprintf(fid, '%f', B(i));
    fprintf(fid, '\n');
end
% write the objective constants if exist
objconst = [];
if exist('c00', 'var') == 1
    if exist('c0', 'var') == 1
        objconst = c00 - c0;
    else
        objconst = c00;
    end
else
    if exist('c0', 'var') == 1
        objconst = -c0;
    else
        objconst = [];
    end
end
if ~isempty(objconst)
    if objconst ~= 0
        fprintf(fid, '%s', '    RHS1');
        fprintf(fid, '%s', '      ');
        fprintf(fid, '%s', 'OBJ');
        fprintf(fid, '%s', '       ');
        fprintf(fid, '%f', objconst);
        fprintf(fid, '\n');
    end
end
% create RANGES section
if isempty(R) ~= 1
    fprintf(fid, '%s', 'RANGES'); % print the RANGES header
    fprintf(fid, '\n');
    [a, ~] = size(R);
    for i = 1:a % print all ranges
        fprintf(fid, '%s', '    RANGE1');
        fprintf(fid, '%s', '    ');
        fprintf(fid, '%s', 'R');
        fprintf(fid, '%i', R(i, 1));
        szcount = num2str(R(i, 1));
        y = length(szcount);
        k = 5 - y;
        for m = 1:k
            fprintf(fid, '%s', ' ');
        end
        fprintf(fid, '%s', '  ');
        if Eqin(R(i, 1)) == -1
            if R(i, 4) == 1
                fprintf(fid, '%f', -(R(i, 2) - B(R(i, 1))));
                fprintf(fid, '\n');
            else
                fprintf(fid, '%f', R(i, 2) - B(R(i, 1)));
                fprintf(fid, '\n');
            end
        elseif Eqin(R(i, 1)) == 0
            if R(i, 4) == 1
                fprintf(fid, '%f', R(i, 3) - B(R(i, 1)));
                fprintf(fid, '\n');
            else
                fprintf(fid, '%f', R(i, 2) - B(R(i, 1)));
                fprintf(fid, '\n');
            end
        elseif Eqin(R(i, 1)) == 1
            if R(i, 4) == 1
                fprintf(fid, '%f', R(i, 3) - R(i, 2));
                fprintf(fid, '\n');
            else
                fprintf(fid, '%f', -(R(i, 3) - R(i, 2)));
                fprintf(fid, '\n');
            end
        end
    end
end
% create BOUNDS section
if isempty(BS) ~= 1
    fprintf(fid, '%s', 'BOUNDS'); % print the BOUNDS header
    fprintf(fid, '\n');
    a = length(BS);
    for i = 1:a % print each bound
        if BS(i, 2) == -1
            fprintf(fid, '%s', ' UP');
        elseif BS(i, 2) == 1
            fprintf(fid, '%s', ' LO');
        elseif BS(i, 2) == 0
            fprintf(fid, '%s', ' FX');
        elseif BS(i, 2) == 11
            fprintf(fid, '%s', ' FR');
        elseif BS(i, 2) == 111
            fprintf(fid, '%s', ' MI');
        elseif BS(i, 2) == 1111
            fprintf(fid, '%s', ' PL');
        end
        fprintf(fid, '%s', ' ');
        fprintf(fid, '%s', 'BOUND1    ');
        fprintf(fid, '%s', 'X');
        fprintf(fid, '%i', BS(i, 1));
        % write the value field for LO, UP, and FX type of
        % constraints
        if BS(i, 2) == -1 || BS(i, 2) == 1 || BS(i, 2) == 0
            szcount = num2str(i);
            y = length(szcount);
            k = 7 - y;
            for m = 1:k
                fprintf(fid, '%s', ' ');
            end
            fprintf(fid, '%s', '  ');
            fprintf(fid, '%f', BS(i,3));
            fprintf(fid, '\n');
        else
            fprintf(fid, '\n');
        end
    end
end
% write the ENDATA header
fprintf(fid, '%s', 'ENDATA');
fclose(fid); % close the file
delete matlab.mat % delete matlab.mat
end