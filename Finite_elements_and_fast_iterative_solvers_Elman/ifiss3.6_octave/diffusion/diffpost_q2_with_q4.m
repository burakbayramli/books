function [error_ele,fe,ae] = ... 
          diffpost_q2_with_q4(xy,mv,mbound,q2sol,eex,hx,hy,neumannb,reduction_level)
%DIFFPOST_Q2_WITH_Q4 local Poisson error estimator for Q2 solution 
%   [errorsq_ele,fe,ae] = ...
%   diffpost_q2_with_q4(xy,mv,mbound,q2sol,eex,hx,hy,reduction_level);
%   input
%          xy             vertex coordinate vector  
%          mv              element mapping matrix
%          mbound          element edge boundary matrix 
%          q2sol           vertex solution vector
%          eex             element edge connectivity array
%          hx,hy           element mesh sizes  
%          neumannb        Neumann boundary element/edge array
%          reduction_level reduction level of the Q4 correction space
%   output
%          errorsq_ele     element error estimate
%          fe              elementwise rhs vectors
%          ae              LDLT factorized element matrices
%
%   calls functions q2fluxjmps.m, gausspoints_oned, gausspoints_twod, gauss_source
%   IFISS function: DJS; 28 September 2013.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
      fprintf('computing Q2_Q4 error estimator...  ')
      x=xy(:,1); y=xy(:,2);
      mel=length(mv(:,1));
      error_ele=zeros(mel,1);
% construct the integration rule
ngpt=7;
[oneg,onew] = gausspoints_oned(ngpt);
[s,t,wt] = gausspoints_twod(oneg,onew);
nngpt=ngpt^2; ng=ngpt;     
%
% ---------------------------------------------    
% Q4 element node numbering 
%           4     15    7     14    3 
%                 
%           16    24    23    22    13
%                 
%           8     25    9     21    6                                  
%                                                  
%           17    18    19    20    12
%                                                                 
%           1     10    5     11    2
% ---------------------------------------------
%
fprintf('reduction level %d \n', reduction_level);
if reduction_level==1
error_node=[1;2;3;4;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25]; 
elseif reduction_level==2
error_node=[10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25]; 
elseif reduction_level==3
error_node=[10;11;12;13;14;15;16;17;18;20;22;24]; 
else
     error('reduction level invalid!')
end
N_error_node=length(error_node); nn=N_error_node;
tic
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(mv(:,ivtx));
        yl_v(:,ivtx) = y(mv(:,ivtx)); 
        end
        for ivtx=1:9
           sl_v(:,ivtx)=q2sol(mv(:,ivtx));
        end
        ae = zeros(mel,N_error_node,N_error_node); elerr=zeros(N_error_node,mel);
        fe = zeros(mel,N_error_node);
% loop over Gauss points
      for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
% evaluate derivatives etc
         [jac_v,invjac_v,phi_v,dphidx_v,dphidy_v] = deriv(sigpt,tigpt,xl_v,yl_v);
         rhs_v = gauss_source(sigpt,tigpt,xl_v,yl_v);
         [qua_v,dquadx_v,dquady_v] = qqderiv(sigpt,tigpt,xl_v,yl_v); 
         [dpsi2dx2,dpsi2dy2] = qderiv_2(sigpt,tigpt,xl_v,yl_v);
	     dsl2dx2=zeros(mel,1);
         dsl2dy2=zeros(mel,1);
         for i=1:9
             dsl2dx2=dsl2dx2+sl_v(:,i).*dpsi2dx2(:,i).*invjac_v.^2;
             dsl2dy2=dsl2dy2+sl_v(:,i).*dpsi2dy2(:,i).*invjac_v.^2;
          end
         for j = 1:N_error_node
             jj=error_node(j);
             for i = 1:N_error_node
                 ii=error_node(i);
                ae(:,i,j) = ae(:,i,j)+wght*dquadx_v(:,ii).*dquadx_v(:,jj).*invjac_v(:);
                ae(:,i,j) = ae(:,i,j)+wght*dquady_v(:,ii).*dquady_v(:,jj).*invjac_v(:);
             end
	         fe(:,j) = fe(:,j)  +  wght* (rhs_v+dsl2dx2+dsl2dy2) .* qua_v(:,jj) .* jac_v(:);          
          end
% end of Gauss point loop
      end
%
% include edge jumps
         for ee = 1:N_error_node
             for igpt=1:ng
                  sigpt=oneg(igpt);
                  wigpt=onew(igpt);
                  jmp = q2fluxjmps(q2sol,eex,xy,mv,mbound,neumannb,sigpt);
                  eei=error_node(ee);
                  if eei==1| eei==10| eei==5| eei==11| eei==2
                    [qua_v,dquadx_v,dquady_v] = qqderiv(sigpt,-1,xl_v,yl_v);                
                    fe(:,ee) = fe(:,ee) - wigpt*1/2.*jmp(:,1).*qua_v(:,eei) .* hx(:)./2;
                  end
                  if eei==2| eei==12| eei==6| eei==13| eei==3
                    [qua_v,dquadx_v,dquady_v] = qqderiv(1,sigpt,xl_v,yl_v);
                    fe(:,ee) = fe(:,ee) - wigpt*1/2.*jmp(:,2).*qua_v(:,eei) .* hy(:)./2;
                  end
                  if eei==3| eei==14| eei==7| eei==15| eei==4
                    [qua_v,dqardx_v,dqardy_v] = qqderiv(sigpt,1,xl_v,yl_v);
                    fe(:,ee) = fe(:,ee) - wigpt*1/2.*jmp(:,3).*qua_v(:,eei) .* hx(:)./2;
                  end
                 if eei==4| eei==16| eei==8| eei==17| eei==1
                    [qua_v,dquadx_v,dquady_v] = qqderiv(-1,sigpt,xl_v,yl_v);
                    fe(:,ee) = fe(:,ee) - wigpt*1/2.*jmp(:,4).*qua_v(:,eei) .* hy(:)./2;
                  end   
 %               
             end
         end

% solve for local estimate (sequential code)
%         for ielem = 1:mel
%		    elerr(:,ielem) = squeeze(ae(ielem,1:nnode,1:nnode))\(fe(ielem,1:nnode)'); 
%         end
%         for ivtx=1:nnode, 
%	        elerr_p(:) = elerr_p(:) + fe(:,ivtx) .* elerr(ivtx,:)';
%         end

% vectorized code
% LDLT factorization      
nel=mel;
dd=zeros(nel,nn); rr=zeros(nel,nn);
for kk=1:nn-1
    for pp=1:kk-1;
    rr(1:nel,pp)=dd(1:nel,pp).*ae(1:nel,kk,pp);
    end
    dd(1:nel,kk)=ae(1:nel,kk,kk);
    for pp=1:kk-1;
    dd(1:nel,kk)= dd(1:nel,kk)- ae(1:nel,kk,pp).*rr(1:nel,pp);
    end
    for ii=kk+1:nn
        for pp=1:kk-1;
        ae(1:nel,ii,kk)=ae(1:nel,ii,kk)-ae(1:nel,ii,pp).*rr(1:nel,pp);
        end
        ae(1:nel,ii,kk)=ae(1:nel,ii,kk)./dd(1:nel,kk);
    end
end
    for pp=1:nn-1;
    rr(1:nel,pp)=dd(1:nel,pp).*ae(1:nel,nn,pp);
    end
    dd(1:nel,nn)=ae(1:nel,nn,nn);
    for pp=1:nn-1;
    dd(1:nel,nn)= dd(1:nel,nn)- ae(1:nel,nn,pp).*rr(1:nel,pp);
    end
% overwrite diagonal entries
    for kk=1:nn
    ae(1:nel,kk,kk)= dd(1:nel,kk);
    end
%
%  forward-backward substitutions ...
   xx = element_lusolve(ae,fe);
   elerr=xx';  
%
   for ivtx=1:N_error_node
      error_ele(:) = error_ele(:) + fe(:,ivtx) .* elerr(ivtx,:)';
   end	
etime=toc;
fprintf('error estimation took %6.3e seconds\n',etime)  
return
