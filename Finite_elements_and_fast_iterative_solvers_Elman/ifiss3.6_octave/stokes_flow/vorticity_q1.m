function wev = vorticity_q1(xy,ev,w,tout)
%VORTICITY_Q1 Q1 total vorticity
%   wev = vorticity_q1(xy,ev,w,tout);
%   input
%          xy         Q1 nodal coordinate vector 
%          ev         element mapping matrix
%          w          Q1 vorticity vector
%          tout       output on/off switch         
%   output
%          wev     element vorticity vector 
%
%   IFISS function: DJS; 24 November 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage
nngpt=4; 
x=xy(:,1); y=xy(:,2);
nvtx=length(x);   nel=length(ev(:,1));
%
% initialise global matrices
      n = sparse(nvtx,nvtx);
%
%
     if (nngpt==4)        % 2x2 Gauss points
      gpt=1.0e0/sqrt(3.0e0);
      s(1) = -gpt; t(1) = -gpt; wt(1)=1;
      s(2) =  gpt; t(2) = -gpt; wt(2)=1;
      s(3) =  gpt; t(3) =  gpt; wt(3)=1; 
      s(4) = -gpt; t(4) =  gpt; wt(4)=1;
     elseif (nngpt==9)   % 3x3 Gauss points 
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
     elseif (nngpt==1)   % 1x1 Gauss point
      s(1) =    0; t(1) =    0; wt(1)=4;
      else
	  error('Check Gauss point integration specification')
      end

%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
        end
        for idx = 1:4		
		wsl(:,idx) = w(ev(:,idx));
        end
% 
 wev = zeros(nel,1); 
% loop over Gauss points
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
%  evaluate basis function
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
            for k=1:4
		    wev(:) = wev(:) + wght*wsl(:,k) .* phi(:,k).* jac(:); 
		    end
%
% end of Gauss point loop
         end  
%
if tout==1,
wtotal=sum(wev);
fprintf('   total vorticity is %e\n', wtotal)
%fprintf('Plotting vorticity distribution\n')
%%  plot vorticity vector
%     eplot(wev,ev(:,1:4),xy,x,y,69); pause
%fprintf('done.\n')
end
return
