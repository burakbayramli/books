function placeHolder = readEntryLine(fields,dataType,default,row,placeHolder,C)

for i = 1:9
    
    skip = 0;
    % check if input is used
    if isempty(default{row,i})
        % input used
    elseif default{row,i} == Inf
        % skip  unused field
        skip = 1;
    end
    
    if skip == 0;
        % check required fields and defaults for empty field inputs
        if isempty(C{i})
            if isempty(default{row,i})
                error([fields{row,i},' field required in ',fields{1,1},' entries.']);
            else % use default
                if ischar(default{row,i}); C{i} = default{row,i};
                else                       C{i} = num2str(default{row,i});
                end
            end
        end
        placeHolder.(fields{row,i}) = processType(dataType{row,i},C{i});
    end
end

end


function formattedData = processType(dataMapEnt,dataEnt)
if strcmp(dataMapEnt,'str')
    formattedData = dataEnt;
else %strcmp(dataMapEnt,'int') || strcmp(dataMapEnt,'dec') || strcmp(dataMapEnt,'sci');
    formattedData = str2double(dataEnt);
    
    % issues with NaN
    if formattedData <= 0 ||  formattedData > 0
    else
        formattedData = [];
    end
    
end
end