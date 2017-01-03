function [] = loadFileVariablesIntoWS(filename)
    
    % Check if file extension is given, if nt assume ".m"
    if strcmp(filename(end-1), '.') == 0
        filename = [ filename '.m' ];
    end

    fid = fopen(filename);
    if fid == -1
        error(sprintf('ERROR: Cannot find file specified: %s', filename));
    end

    % reads & evaluates the input lines in the file
    while ~feof(fid)
        current_line = fgetl(fid);
        if ~isempty(current_line) & current_line(1) ~= '%'
            evalin('caller',current_line);
        end
    end