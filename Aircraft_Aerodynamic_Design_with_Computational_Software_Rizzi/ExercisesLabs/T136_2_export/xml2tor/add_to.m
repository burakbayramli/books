function tab = add_to(tab,xint,pccon)
% Tornado function (internal): add info to span table of properties
% Input
%   tab   table
%   xint  what
%   pccon lin interp. or piecewise constant
%
% Output
%   tab   table for partitions
%
% calls
% --
%--------------------------------------------------------------------------
%  Revisions: KTH 091022 v 0
% jop 100610 - make look-up work for negative xint
%
[m,n]=size(tab);
for k = 1:m
    if (xint-tab(k,1))*(xint-tab(k,2))<=0 % put row here
        frac = 1-(xint-tab(k,1))/(tab(k,2)-tab(k,1));
        if frac < 0.99999 && frac > 0.000001  % ... the break does not already exist
            % so add a row
            row  = tab(k,:);      % OK for pccon
            tmp  = row(~pccon);   % linear intrp
            tmp2 = frac*tmp(1:2:end-1)+(1-frac)*tmp(2:2:end);
            tmp(2:2:end) = tmp2;
            row(~pccon)  = tmp;
            % insert empty row
            tab = [tab(1:k,:);zeros(1,n);tab(k+1:m,:)];
            % fill it
            tab(k+1,2:2:end  ) = tab(k,2:2:end);
            tab(k+1,1:2:end-1) = row(2:2:end);
            % modify prev. row
            tab(k  ,2:2:end  ) = row(2:2:end);
        end
        break
    end
end
end