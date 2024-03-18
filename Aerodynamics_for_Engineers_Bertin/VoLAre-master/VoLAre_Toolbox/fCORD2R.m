function varargout = fCORD2R(varargin)
func = varargin{1};
switch func
    case 'readline'
        varargout{1} = readline(varargin{2},varargin{3},varargin{4});
        
        
    case 'minRows'
        varargout{1} = 2;
    case 'maxRows'
        varargout{1} = 2;
    otherwise
        error('invalid input option')
end

end


function placeHolder = readline(row,placeHolder,C)

fields = {'CORD2R','CID','RID','A1','A2','A3','B1','B2','B3';
[],'C1','C2','C3',[],[],[],[],[]};

dataType = {'str','int','int','dec','dec','dec','dec','dec','dec';
    [],'dec','dec','dec',[],[],[],[],[]};

default = {Inf,[],Inf,[],[],[],[],[],[];
Inf,[],[],[],Inf,Inf,Inf,Inf,Inf};

placeHolder =  readEntryLine(fields,dataType,default,row,placeHolder,C) ;
end