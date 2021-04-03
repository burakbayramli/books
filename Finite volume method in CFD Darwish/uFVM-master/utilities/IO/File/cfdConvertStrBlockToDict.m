function dict = cfdConvertStrBlockToDict(block_str)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   This function checks if a block exists in the string str, and returns a
%   dict containing the keys and values available in str
%--------------------------------------------------------------------------

dict = struct;

% Check if block contains sub-blocks
indices = strfind(block_str, '{');
if length(indices)>1
    
    opening_pos = strfind(block_str, '{'); opening_pos = opening_pos(1);
    closing_pos = strfind(block_str, '}'); closing_pos = closing_pos(end);
    
    block_str = block_str(opening_pos+1:closing_pos-1);
    
    bracelets = [];
    bracelets_pos = [];
    for i=1:length(block_str)
        if strcmp(block_str(i), '{')
            bracelets = [bracelets, '{'];
            bracelets_pos = [bracelets_pos, i];
        end
        if strcmp(block_str(i), '}')
            bracelets = [bracelets, '}'];
            bracelets_pos = [bracelets_pos, i];
        end
    end
    
    pairs = [];
    pair_index = 1;
    for i=1:length(bracelets)
        if strcmp(bracelets(i),'{')
            pairs = [pairs pair_index];
            pair_index = pair_index + 1;
        else
            pair_index = pair_index - 1;
            pairs = [pairs pair_index];
        end
    end
    
    C = cell(length(bracelets)/2,1);
    k = 1;
    paired = -1*ones(1,length(bracelets));
    for i=1:length(bracelets)
        if strcmp(bracelets(i),'}')
            continue;
        end
        for j=i+1:length(bracelets)
            if pairs(j)==pairs(i) && paired(j)==-1
                C{k,1} = [i, j];
                paired([i,j]) = 1;
                k = k +1;
                break;
            end
        end
        if k==length(bracelets)/2+1
            break;
        end
    end
    
    removeInterval = [];
    for i=1:length(C)
        if pairs(C{i,1})~=1
            continue;
        end
        % Add block name to removal interval
        C_blockname = strsplit(block_str(1:bracelets_pos(C{i,1}(1))-1));
        l = length(C_blockname);
        for j=2:l
            if isempty(C_blockname{j})
                C_blockname(j) = [];
                j = j - 1;
                l = l - 1;
            end
        end
        if isempty(C_blockname{1})
            C_blockname(1) = [];
        end
        
        blockName = C_blockname{end};
        if ~isempty(strfind(blockName, '}'))
            ind = strfind(blockName, '}');
            ind = ind(end);
            blockName = blockName(ind+1:end);
        end
        
        if ~isempty(strfind(blockName, ';'))
            ind = strfind(blockName, ';');
            ind = ind(end);
            blockName = blockName(ind+1:end);
        end
        
        ind = strfind(block_str, blockName); ind = ind(end);
        removeInterval = [removeInterval ind:ind+length(blockName)-1];
        
        % Remove block
        removeInterval = [removeInterval bracelets_pos(C{i,1}(1)):bracelets_pos(C{i,1}(2))];
        
        % Read block
        dict.(blockName) = cfdConvertStrBlockToDict(block_str(bracelets_pos(C{i,1}(1)):bracelets_pos(C{i,1}(2))));
    end
    
    block_str(removeInterval) = [];
    
    if ~cfdContains(block_str, ';')
        return;
    end
    
    C_str = strsplit(block_str, ';');
    for i=1:length(C_str)
        if isempty(strtrim(C_str{i}))
            continue;
        end
        
        % Get key
        key = textscan(C_str{i}, '%s', 1); key = key{1}{1};
        key_start = strfind(C_str{i}, key);
        if length(key_start)>1
            key_start = key_start(1);
        end
        
        % Get value
        value = strtrim(C_str{i}((key_start+length(key)):end));
        
        if isempty(value)
            continue;
        end
        
        % Store
        dict.(key) = value;
    end
else
    opening_pos = strfind(block_str, '{');
    closing_pos = strfind(block_str, '}');
    
    C_str = strsplit(block_str(opening_pos+1:closing_pos-1), ';');
    for i=1:length(C_str)
        if isempty(strtrim(C_str{i}))
            continue;
        end
        
        % Get key
        key = textscan(C_str{i}, '%s', 1); key = key{1}{1};
        key_start = strfind(C_str{i}, key);
        if length(key_start)>1
            key_start = key_start(1);
        end
        
        % Get value
        value = strtrim(C_str{i}((key_start+length(key)):end));
        
        % Store
        dict.(key) = value;
    end
end
