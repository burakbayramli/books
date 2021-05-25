function [dc, rc] = cfdFactorizeILU(ac,anb,bc,cconn)
%--------------------------------------------------------------------------
%
%  Written by the CFD Group @ AUB, Fall 2018
%  Contact us at: cfd@aub.edu.lb
%==========================================================================
% Routine Description:
%   Incomplete Lower Upper Factorization
%--------------------------------------------------------------------------

numberOfElements = length(ac);

dc = zeros(size(ac));
rc = zeros(size(ac));

for i1=1:numberOfElements
    dc(i1) = ac(i1);
end

for i1=1:numberOfElements
    dc(i1) = 1.0/dc(i1);
    rc(i1) = bc(i1);
    
    i1NbList = cconn{i1};
    i1NNb = length(i1NbList);
    
    if(i1~=numberOfElements-1)
        % loop over neighbours of iElement
        j1_ = 1;
        while(j1_<=i1NNb)
            jj1 = i1NbList(j1_);
            % for all neighbour j > i do
            if((jj1>i1) && (jj1<=numberOfElements))
                j1NbList = cconn{jj1};
                j1NNb = length(j1NbList);
                i1_= 0;
                k1 = -1;
                % find _i index to get A[j][_i]
                while((i1_<=j1NNb) && (k1 ~= i1))
                    i1_ = i1_ + 1;
                    k1 = j1NbList(i1_);
                end
                % Compute A[j][i]*D[i]*A[i][j]
                if(k1 == i1)
                    dc(jj1) = dc(jj1) - anb{jj1}(i1_)*dc(i1)*anb{i1}(j1_);
                else
                    disp('the index for i in j is not found');
                end
            end
            j1_ = j1_ + 1;
        end
    end
end