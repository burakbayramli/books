function varargout = fACOORD(varargin)
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

fields = {'ACOORD','ID','XORIGN','YORIGN','ZORIGN','DELTA','THETA','',''};
dataType = {'str','int','dec','dec','dec','dec','dec',[],[]};
default = {Inf,[],[],[],[],[],[],Inf,Inf};

placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C) ;
end