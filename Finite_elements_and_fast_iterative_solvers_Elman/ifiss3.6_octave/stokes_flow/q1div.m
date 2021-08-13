
function ediv = q1div(xy,ev,flowsol)
%Q1DIV computes norm of divergence of Q1 flow solution (corrected)
%   ediv = q1div(xy,ev,flowsol);
%   input
%          xy         vertex coordinate vector  
%          ev         element mapping matrix
%          flowsol    Q1 flow solution vector
%
%   IFISS function: DJS; 16 August 2017.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      x=xy(:,1); y=xy(:,2); nvtx=length(x);
      nel=length(ev(:,1)); 
	  usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
      fprintf('computing divergence of discrete velocity solution ...  ')

%
% initialise global matrices
       ediv=zeros(nel,1);
%
% set up 2x2 Gauss points
      gpt=1.0e0/sqrt(3.0e0);
      s(1) = -gpt;  t(1) = -gpt;
      s(2) =  gpt;  t(2) = -gpt;
      s(3) =  gpt;  t(3) =  gpt;
      s(4) = -gpt;  t(4) =  gpt;
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
        xsl(:,ivtx) = usol(ev(:,ivtx));
		ysl(:,ivtx) = vsol(ev(:,ivtx));
		end
% loop over 2x2 Gauss points
         for igpt = 1:4
         sigpt=s(igpt);
         tigpt=t(igpt);
%  evaluate derivatives 
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
		 evec=zeros(nel,1);
            for j=1:4
		    evec(:) = evec(:) + xsl(:,j) .* dphidx(:,j) + ysl(:,j) .* dphidy(:,j);
		    end
            ediv(:)=ediv(:) + evec(:).*evec(:).*invjac(:);
% end of Gauss point loop
         end
%
% end of element loop
%
      err_div = sqrt(sum(ediv)); ediv = sqrt(ediv);
fprintf('done\n')
fprintf('estimated velocity divergence error:  %10.6e \n',err_div) 
return
