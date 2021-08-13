%HELPME IFISS interactive help facility
%   IFISS scriptfile: DJS; 9 January 2019.
% Copyright (c) 2019 D.J. Silvester, H.C. Elman, A. Ramage (see readme.m)

fprintf(' \n');
fprintf(' IFISS\n')
fprintf('Copyright (c) 2019 by D.J. Silvester, H.C. Elman and A. Ramage (see readme.m)\n')
%fprintf(' \n');
fprintf('To install the toolbox, run the script-file install_ifiss.m\n');
%fprintf(' \n');

fprintf(' (Type any character to continue.)')
pause;

fprintf('\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b');
fprintf('\nTry running a software demo to get started \n'); help ifissdemos

fprintf('\n For further information on\n');
fprintf('    special features                                  enter 0\n');
fprintf('    solving a diffusion problem                             1\n');
fprintf('    solving a Stokes flow problem                           3\n');
fprintf('    solving an optimization problem                         5\n');
fprintf('    solving a convection-diffusion problem                  6\n');
fprintf('    solving a Navier-Stokes flow problem                    8\n\n');
fprintf('    general information on time-dependent problems          10\n');
fprintf('    solving a Boussinesq flow problem                       11\n');
fprintf('    solving a biharmonic problem                            13\n\n');
fprintf('    exploring multigrid solvers                             2\n');
fprintf('    exploring preconditioned Krylov subspace solvers        99\n\n');
hlp=default('    Help topic',-1);
if hlp==1, gohome; cd diffusion; helpme_diff;  gohome, cd timestepping; helpme_heat; 
elseif hlp==6, gohome; cd convection; helpme_cd; gohome; cd timestepping; helpme_unsteady_cd;
elseif hlp==3, gohome; cd stokes_flow; helpme_stokes; helpme_stopping
elseif hlp==8, gohome; cd navier_flow; helpme_navier; gohome; cd timestepping; helpme_unsteady_navier;
elseif hlp==99, gohome; cd solvers; helpme_it;
elseif hlp==2, gohome; cd solvers; helpme_mg;
elseif hlp==10, gohome; cd timestepping; helpme_timestepping;
elseif hlp==11, gohome; cd boussinesq_flow; helpme_bouss;
elseif hlp==5, gohome; cd pde_control; helpme_poissoncontrol;
elseif hlp==13, gohome; cd biharmonic; helpme_biharm;
elseif hlp==0, 
   gohome; helpme_ch47;
   if exist('djs','file') == 7,
      gohome; helpme_djs;
   end
   if exist('hce','file') == 7,
      gohome; helpme_hce;
   end
   if exist('ar','file') == 7,
      gohome; helpme_ar;
   end
end
gohome;
fprintf(' \n');


