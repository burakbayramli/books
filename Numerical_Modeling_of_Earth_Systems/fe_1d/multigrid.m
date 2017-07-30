%
% multigrid solver, V cycle implementation
%
% solved s(levmax).stiffness . sol = s(levmax).load
%
% max_iter1 should be a small number for the approximate solutions
% max_iter2 should be a large number for the close to exact solution
%           of the lowest level
%
function [ sol iter residue ] = multigrid(s,levmax,tol,max_iter1,max_iter2)

gstol = min(1e-7,tol); % this not used, since max_iter values should lead to earlier bailout

accuracy = tol*norm(s(levmax).load);
%
% local solution structure
for lev=levmax:-1:1			% initialize
  mneq = s(lev).neq;
  ss(lev).neq = mneq;
  ss(lev).u     = zeros(mneq,1);
  ss(lev).res   = zeros(mneq,1);
  ss(lev).del_u = zeros(mneq,1);
end

%
% use notation K d = F 
%
sol = zeros(s(levmax).neq,1);
%
ss(levmax).res = s(levmax).load;		% R = F for top level

counts=0;
residue = accuracy * 10;
while(residue > accuracy)
  for lev=levmax:-1:2			% go top down
    %
    % solve K d = F at top level with a few iterations
    %
    [ ss(lev).u iter1 res1 ] = ...
        gauss_seidel(s(lev).stiffness,ss(lev).res,...
        zeros(ss(lev).neq,1),gstol,max_iter1);
    %
    % misfit after max_iter1 iterations, R_n = F_n - K_n d_n
    %
    ss(lev).res = ss(lev).res - s(lev).stiffness * ss(lev).u;
    %
    % project residual down from level lev to coarser level lev-1
    ss(lev-1).res = project(ss(lev).res,lev,[s(:).neq]);
  end
  % solve at lowest level, could also do
  % ss(1).u = s(1).stiffness\ss(1).res
  %
  [ ss(1).u iter2 res2 ] = ...
      gauss_seidel(s(1).stiffness,ss(1).res,zeros(ss(1).neq,1),...
      gstol,max_iter2);

  %
  % go back up 
  %
  for lev=2:levmax
    %
    % interpolate up
    %
    ss(lev).del_u = interp(ss(lev-1).u,lev-1,[s(:).neq]);
    %
    % solve anew, but start from interpolated del_u(lev)
    %
    [ ss(lev).del_u iter3 res2 ] = ...
        gauss_seidel(s(lev).stiffness,ss(lev).res,ss(lev).del_u,...
        gstol,max_iter1);
    del_R = - s(lev).stiffness * ss(lev).del_u;
    %
    % scale for addition
    %
    alpha = (del_R' * ss(lev).res)/(del_R'*del_R);
    %
    % add increment to the old solution
    %
    ss(lev).u   =  ss(lev).u   - alpha * ss(lev).del_u;
    ss(lev).res =  ss(lev).res - alpha * del_R;
  end

  sol = sol + ss(levmax).u;

%  plot(sol),drawnow
  residue = norm(ss(levmax).res)
  counts=counts+1;
end

iter = counts;

end

%
% interpolation has to implment boundary condition
%
% project residual down from level lev to coarser level 
% from res(lev) to lev-1
function resl =  project(res,lev,neq)

resl = zeros(neq(lev-1),1);
for i=2:neq(lev-1)
    j=(i-1)*2+1;
    if(j == neq(lev))
        resl(i)=(2*res(j) + res(j-1))/3.0;
    else
        resl(i)=(res(j+1) + 2*res(j) + res(j-1))/4.0;
    end
end
%  apply to BC at x=0 
resl(1) = 0.0;

end


%
% interpolate up from res(lev) to lev+1
%
function resu = interp(res,lev,neq)

resu=zeros(neq(lev+1),1);

% first inject resL onto upper level 
for j=1:neq(lev)
  i = 2*j-1;
  resu(i) = res(j);
end
% then do the averages 
for i=2:2:neq(lev+1)
  resu(i) = (resu(i-1)+resu(i+1))*0.5;
end

% apply to BC at x=0 
resu(1) = 0.0;
end
