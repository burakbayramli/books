function foamDict = cfdSplitStrBlockFile(block_str)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%
%--------------------------------------------------------------------------

foamDict = struct;

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

for i=1:length(C)
    if pairs(C{i,1})~=1
        continue;
    end
    
    opening = bracelets_pos(C{i,1}(1));
    closing = bracelets_pos(C{i,1}(2));
        
    % Add inline keys and values
    dict = cfdConvertStrBlockToDict(block_str(opening:closing));
    
    % Get block name
    C_blockname = strsplit(block_str(1:opening-1));
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
    
    if ~isempty(dict)
        foamDict.(blockName) = dict;
    end    
end
