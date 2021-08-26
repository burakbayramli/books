function [um,up] = WENO(xloc,uloc,m,Crec,dw,beta);
% Purpose: Compute the left and right cell interface values using an WENO
%          approach based on 2m-1 long vectors uloc with cell
% Set WENO parameters
p = 1; q = m-1; vareps = 1e-6;

% Treat special case of m=1 - no stencil to select
if (m==1)
    um = uloc(1); up = uloc(1);
else
    alpham = zeros(m,1); alphap = zeros(m,1); 
    upl = zeros(m,1); uml = zeros(m,1); betar = zeros(m,1);

    % Compute um and up based on different stencils and 
    % smoothness indicators and alpha
    for r=0:m-1;
        umh = uloc(m-r+[0:m-1]);
        upl(r+1) = Crec(r+2,:)*umh; uml(r+1) = Crec(r+1,:)*umh;
        betar(r+1) = umh'*beta(:,:,r+1)*umh;
    end;
    
    % Compute alpha weights - classic WENO
    alphap = dw./(vareps+betar).^(2*p); 
    alpham = flipud(dw)./(vareps+betar).^(2*p); 
    
%   % Compute alpha weights - WENO-Z
%     tau = abs(betar(1) - betar(m));
%     if mod(m,2)==0
%         tau = abs(betar(1)-betar(2) - betar(m-1) + betar(m));
%     end
%     alphap = dw.*(1 + (tau./(vareps+betar)).^q);
%     alpham = flipud(dw).*(1 + (tau./(vareps+betar)).^q);

    % Compute nonlinear weights and cell interface values
    um=alpham'*uml/sum(alpham); up=alphap'*upl/sum(alphap);
end
return