function [ok,data]=isinptok(data,type)%is input ok?
%  Author: Tomas Melin <dr.tomas.melin@gmail.com>
%  Keywords: Input checker.
%
% Revision History:
%   Råberga, 2016-11-24 :  move to input function.    
%   Bristol, 2007-06-27 :  Addition of new header. TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if type==1; %data should be a numerics
    if ischar(data)
        if sum(strcmp(data,{'b' 'B'}));		%is data 'b' or 'B'? 
            ok=-1;										%signal back menu
            return
        elseif sum(strcmp(data,{'q' 'Q'}));	%is data 'q' or 'Q'?       
            ok=-2;										%signal escape menu
            return
        elseif isempty(str2num(data));      %Is data another string?
            ok=0;
            return
        end
    end
    if isempty(data)
        ok=0;
        return
    end
    
    
elseif type==2 %Data should be a string
          if ischar(data)
        if sum(strcmp(data,{'b' 'B'}));		%is data 'b' or 'B'? 
            ok=-1;										%signal back menu
            return
        elseif sum(strcmp(data,{'q' 'Q'}));	%is data 'q' or 'Q'?       
            ok=-2;										%signal escape menu
            return
        end
    end
    if isempty(data)
        ok=0;
        return
    end  
    
    
end




data=data(1);
ok=1;
end