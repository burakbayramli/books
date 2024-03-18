function varargout = fPANLST2(varargin)
func = varargin{1};
switch func
    case 'readline'
        varargout{1} = readline(varargin{2},varargin{3},varargin{4});
        
    case 'minRows'
        varargout{1} = 1;
    case 'maxRows'
        varargout{1} = Inf;
    otherwise
        error('invalid input option')
end

end


function placeHolder = readline(row,placeHolder,C)

if row < 2
    
    % fields = {'AEFACT','SID','VALUE1','VALUE2','VALUE3','VALUE4','VALUE5','VALUE6','VALUE7'};
    % dataType = {'str','int','dec','dec','dec','dec','dec','dec','dec'};
    % default = {Inf,'','','','','','','',''};
    
    if isempty(C{2}); error('SETID required in PANLST2 entries.'); end
    if isempty(C{3}); error('MACROID required in PANLST2 entries.'); end
    
    placeHolder.SETID = str2num(C{2});
    placeHolder.MACROID = str2num(C{3});
        
        
    vi = 0;
    while vi < 6
        vi = vi + 1;
        if size(C{vi+3},2)<1
            break
        end
        placeHolder.VALUE(vi) = str2double(C{vi+3});
    end
    
else
    vi = size(placeHolder.VALUE,2);
    ci = 1;
    while ci < 9
        vi = vi + 1;
        ci = ci + 1;
        if size(C{ci},2)<1
            break
        end
        placeHolder.VALUE(vi) = str2double(C{ci});
    end
end

end