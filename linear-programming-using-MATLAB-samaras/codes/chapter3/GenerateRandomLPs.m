function GenerateRandomLPs
% Filename: GenerateRandomLPs.m
% Description: Custom menu for creating random LPs
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: GenerateRandomLPs
% 
% Input: None
%
% Output: files that store the generated random LPs

choice = 0;
while choice ~= 4
    % show menu
    choice = menu('Generate Random LPs', ...
        'Generator Initialization', 'Dense Random LPs', ...
        'Sparse Random LPs', 'Exit');
	if choice == 1 % initialize generator
        seedNumber = input('Please give the seed number: ');
        rand('state', seedNumber);
        sprand('state');
    elseif choice == 2 % create dense random LPs
        m = input('Please give the number of constraints: ');
        n = input('Please give the number of variables: ');
        denseMenu(m, n);
    elseif choice == 3 % create sparse random LPs
        m = input('Please give the number of constraints: ');
        n = input('Please give the number of variables: ');
        sparseMenu(m, n);
    elseif choice == 4 % exit
        return;
	end
end
end