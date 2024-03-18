function varargout = fTRIMVAR(varargin)
func = varargin{1};
switch func
    case 'readline'
        varargout{1} = readline(varargin{2},varargin{3},varargin{4});
        
        
    case 'minRows'
        varargout{1} = 1;
    case 'maxRows'
        varargout{1} = 1;
    otherwise
        error('invalid input option')
end

end


function placeHolder = readline(row,placeHolder,C)

fields =   {'TRIMVAR','IDVAR','LABEL','LOWER','UPPER','TRIMLNK','DMI','SYM','INITIAL'};
dataType = {'str'    ,'int'  ,'str'  ,'dec'  ,'dec'  ,'int'    ,'str','str','dec'};
default = {Inf       ,[]     ,[]     ,Inf    ,Inf    ,Inf      ,Inf  ,Inf  ,0.0};

placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C) ;
end