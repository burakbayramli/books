function result = fturns(y,in)
% PURPOSE: finds turning points in a time-series
%---------------------------------------------------
% USAGE: result = fturns(y,in)
% where:  y = an (nobs x 1) time-series vector
%        in = a structure variable with options
%          in.but,  # of down-periods before an upturn  (default = 4)
%          in.aut,  # of up-periods   after  an upturn  (default = 1)
%          in.bdt,  # of up-periods   before a downturn (default = 4)
%          in.adt,  # of down-periods after  a downturn (default = 1)
%          in.seq,  1 = sequence inequality,    default = 1 
%                   0 = simple inequaltiy 
%          in.eq    1 = inequality with <=, >=, default = 1
%                   0 = strict inequalty <, >
%   e.g. in.seq=0, in.eq=1 in.but = 3, in.aut = 1 would define:
%   y(t-3), y(t-2), y(t-1), >= y(t)     [but=3, eq=1, seq=0]
%   and y(t+1) > y(t) as UT             [aut=1]
%   and y(t+1) <=  y(t) as NUT          
%
%   e.g. in.seq=0, in.eq=1 in.bdt = 3, in.adt = 2 would define:
%   y(t-3), y(t-2), y(t-1), <= y(t)     [bdt=3, eq=1, seq=0]
%   and y(t+2), y(t+1) < y(t) as DT     [adt=2]
%   and y(t+2), y(t+1) >= y(t) as NDT   
%
%   e.g. in.seq=1, in.eq=1, in.but = 3, in.aut = 1 would define:
%   y(t-3) >= y(t-2) >= y(t-1) >= y(t)  [but=3, eq=1, seq=1]
%   and y(t+1) > y(t) as UT             [aut=1]
%   and y(t+1) <=  y(t) as NUT          
%
%   e.g. in.seq=1, in.eq=0, in.bdt = 3, in.adt = 2 would define:
%   y(t-3) < y(t-2) < y(t-1) < y(t)     [bdt=3, eq=0, seq=1]
%   and y(t+2) > y(t+1) < y(t) as DT    [adt=2]
%   and y(t+2) >= y(t+1) >= y(t) as NDT 
%---------------------------------------------------
% RETURNS:
%        results = a structure variable
%        results.ut  = (nobs x 1) vector with 1 = UT periods
%        results.dt  = (nobs x 1) vector with 1 = DT periods
%        results.nut = (nobs x 1) vector with 1 = NUT periods
%        results.ndt = (nobs x 1) vector with 1 = NDT periods
%        results.y   = time-series vector input
% (NUT = no upturn, NDT = no downturn)        
%--------------------------------------------------
% SEE ALSO: plt_turns (which will plot turning points)
%--------------------------------------------------
% REFERENCES:  Wecker, William E.  (1979), 'Predicting the Turning Points of 
% a Time Series,' Journal of Business, 55, pp.  57-85.

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(in)
error('fturns: structure variable needed for input options');
end;

% set up defaults
but = 4; aut = 1; bdt = 4; adt = 1;
eq = 1; seq=1;
% parse input options
fields = fieldnames(in);
nf = length(fields);
for i=1:nf
    if strcmp(fields{i},'but')
        but = in.but; 
    elseif strcmp(fields{i},'aut')
        aut = in.aut;
    elseif strcmp(fields{i},'bdt');
        bdt = in.bdt;
    elseif strcmp(fields{i},'adt');
        adt = in.adt;
    elseif strcmp(fields{i},'seq');
        seq = in.seq;
    elseif strcmp(fields{i},'eq');
        eq = in.eq;        
    end;
end;

[nobs junk] = size(y);
result.meth = 'fturns';
result.ut  = zeros(nobs,1);
result.dt  = zeros(nobs,1);
result.nut = zeros(nobs,1);
result.ndt = zeros(nobs,1);
result.y = y;

start = max(bdt,but);
send = max(adt,aut);

switch(seq)


case {0} % non-sequential definition

for i=start+1:nobs-send;
    
% find y(i-bdt,1), y(i-bdt-1), y(i-bdt-2), y(i-bdt-3), ... <= y(i)
% which is the condition for a downturn
% find y(i+adt), y(i+adt-1), ... , y(i+1) <= y(i) 
% which is a downturn
% else we have ndt
    
    cntd = 0;
  for j=1:bdt
   if eq == 1
    if y(i-j,1) <= y(i,1)
        cntd = cntd + 1;
    end; % end of if
   elseif eq == 0
    if y(i-j,1) < y(i,1)
        cntd = cntd + 1;
    end; % end of if
   else
    error('fturns: wrong eq setting');
   end;
  end; % end of for j

 if cntd == bdt % conditions right for a downturn
    check = 0;
    for k=1:adt
        if y(i+k,1) < y(i,1)
            check = check + 1;
        end; % end of if
    end; % end of for k
    if check == adt    
     result.dt(i,1) = 1;
    else
     result.ndt(i,1) = 1;
    end; % end of if
 end; % end of if cntd

% find y(i-but,1), y(i-but-1), y(i-but-2), y(i-but-3), ... >= y(i)
% which is the condition for a upturn
% find y(i+aut), y(i+aut-1), ..., y(i+1) <= y(i) 
% which is an upturn
% else we have nut

     cntu = 0;
  for j=1:but
   if eq == 1
    if y(i-j,1) >= y(i,1)
        cntu = cntu + 1;
    end; % end of if
   elseif eq == 0
    if y(i-j,1) > y(i,1)
        cntu = cntu + 1;
    end; % end of if  
   else
    error('fturns: wrong eq setting');
   end;
  end; % end of for j
 if cntu == but % conditions right for an upturn   
    check = 0;
    for k=1:aut
        if y(i+k,1) > y(i,1)
            check = check + 1;
        end; % end of if
    end; % end of for k
    if check == aut    
     result.ut(i,1) = 1;
    else
     result.nut(i,1) = 1;
    end; % end of if
 end; % end of if cntu

end; % end of for i loop

case {1}

for i=start+1:nobs-send;
    
% find y(i-bdt,1) <= y(i-bdt-1) <= y(i-bdt-2) <= y(i-bdt-3) <= ... <= y(i)
% which is the condition for a downturn
% find y(i+adt) <= y(i+adt-1) <= ... <= y(i+1) <= y(i) 
% which is a downturn
% else we have ndt
    
    cntd = 0;
  for j=0:bdt-1
   if eq == 1
    if y(i-j-1,1) <= y(i-j,1)
        cntd = cntd + 1;
    end; % end of if
   elseif eq == 0
    if y(i-j-1,1) < y(i-j,1)
        cntd = cntd + 1;
    end; % end of if
   else
    error('fturns: wrong eq setting');
   end;
  end; % end of for j

 if cntd == bdt % conditions right for a downturn
    check = 0;
    for k=0:adt-1
        if y(i+k+1,1) < y(i+k,1)
            check = check + 1;
        end; % end of if
    end; % end of for k
    if check == adt    
     result.dt(i,1) = 1;
    else
     result.ndt(i,1) = 1;
    end; % end of if
 end; % end of if cntd

% find y(i-but,1) >= y(i-but-1) >= y(i-but-2) >= y(i-but-3) >= ... >= y(i)
% which is the condition for a upturn
% find y(i+aut) <= y(i+aut-1) <= ... <= y(i+1) <= y(i) 
% which is an upturn
% else we have nut

     cntu = 0;
  for j=0:but-1
   if eq == 1
    if y(i-j-1,1) >= y(i-j,1)
        cntu = cntu + 1;
    end; % end of if
   elseif eq == 0
    if y(i-j-1,1) > y(i-j,1)
        cntu = cntu + 1;
    end; % end of if
   else
    error('fturns: wrong eq setting');
   end;
  end; % end of for j
 if cntu == but % conditions right for an upturn   
    check = 0;
    for k=0:aut-1
        if y(i+k+1,1) > y(i+k,1)
            check = check + 1;
        end; % end of if
    end; % end of for k
    if check == aut    
     result.ut(i,1) = 1;
    else
     result.nut(i,1) = 1;
    end; % end of if
 end; % end of if cntu

end; % end of for i loop

% end of non-sequential definition


otherwise
error('fturns: wrong in.seq setting');

end; % end of switch







