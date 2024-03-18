%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%       ======================================================      %
%             Alexander von Essen, Created Apr. 5 2005              %
%                   Last modified: May 2 2005                       %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: muscl.m                                                     %
% Purpose: Returns the value of the limiter function (gamma)        %
% Called by: lr_state.m                                             %
% Calls:                                                            %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function g = muscl(kappa,af,bf,epsilon)
mm = 'minmod';
switch kappa
    case 0
        switch mm
            case 'albada'
                % kappa = 0 beta 1 -- van albada limiter?
                g = 2*(af.*(bf.^2+epsilon)+bf.*(af.^2+epsilon))./(af.^2+bf.^2+2*epsilon);
            otherwise
                g = (sign(af)+sign(bf)).*min(abs(af),abs(bf));
        end
        % JO 1905
        ii = find(af.*bf < 0);
        if ~isempty(ii)
        %    disp(['#: ',num2str(length(ii))])
            g(af.*bf < 0)=0;
        end
    case 3
        % kappa = 1/3
        disp('kappa = 1/3 is not yet implemented. Use kappa = 0.');
    otherwise
        g = 0;
        disp('Invalid kappa value. Setting gamma = 0.');
        return
end
