function ediv = q2divz(xy,mv,flowsol)
%Q2DIVZ legacy code | replaced by corrected q2div.m
%   ediv = q2divz(xy,mv,flowsol);
%   input
%          xy         vertex coordinate vector  
%          mv         Q2 element mapping matrix
%          flowsol    Q2 solution vector
%
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      x=xy(:,1); y=xy(:,2); nvtx=length(x);
      nel=length(mv(:,1)); 
	  usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx);
      fprintf('computing divergence of discrete velocity solution ...  ')

%
% initialise global matrices
       ediv=zeros(nel,1);
%
% set up 3x3 Gauss points
      gpt=sqrt(0.6); 
      s(1) = -gpt; t(1) = -gpt; wt(1)=25/81;
      s(2) =  gpt; t(2) = -gpt; wt(2)=25/81;
      s(3) =  gpt; t(3) =  gpt; wt(3)=25/81; 
      s(4) = -gpt; t(4) =  gpt; wt(4)=25/81;
      s(5) =  0.0; t(5) = -gpt; wt(5)=40/81;
      s(6) =  gpt; t(6) =  0.0; wt(6)=40/81;
      s(7) =  0.0; t(7) =  gpt; wt(7)=40/81; 
      s(8) = -gpt; t(8) =  0.0; wt(8)=40/81;
      s(9) =  0.0; t(9) =  0.0; wt(9)=64/81;
%
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(mv(:,ivtx));
        yl_v(:,ivtx) = y(mv(:,ivtx)); 
        end
        for idx = 1:9		
		xsl(:,idx) = usol(mv(:,idx));
		ysl(:,idx) = vsol(mv(:,idx));
		end
% loop over 3x3 Gauss points
        for igpt = 1:9
        sigpt=s(igpt);
        tigpt=t(igpt);
        wght=wt(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         [psi,dpsidx,dpsidy] = qderiv(sigpt,tigpt,xl_v,yl_v); 
		 evec=zeros(nel,1);
            for j = 1:9
	evec(:) = evec(:) + wght * (xsl(:,j) .* dpsidx(:,j) + ysl(:,j) .* dpsidy(:,j));
		    end
	     ediv(:)=ediv(:) + evec(:).*evec(:);
% end of Gauss point loop
         end
%
% end of element loop
%
      err_div = sqrt(sum(ediv)); ediv = sqrt(ediv);
fprintf('done\n')
fprintf('estimated velocity divergence error:  %10.6e \n',err_div) 
return
