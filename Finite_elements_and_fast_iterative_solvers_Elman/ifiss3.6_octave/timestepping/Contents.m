% TIMESTEPPING
%
% Files
%   box_heat                 - solve Heat Equation in box-shaped domain 
%   dtflowbc                 - imposes updated BC at the current timestep
%   dtheatbc                 - imposes Dirichlet BC at the current timestep
%   ell_heat                 - solve Heat Equation in L-shaped domain 
%   helpme_heat              - heat equation interactive help 
%   helpme_timestepping      - unsteady problem solution interactive help 
%   helpme_unsteady_cd       - unsteady CD problem interactive help 
%   helpme_unsteady_navier   - unsteady flow problem interactive help 
%   resetflowbc              - resets boundary values in unsteady flow 
%   resetheatbc              - resets boundary values in unsteady diffusion 
%   stabtr                   - standard integrator based on stabilized TR
%   stabtrNS                 - Navier-Stokes integrator using stabilized TR 
%   unsteady_cd              - unsteady CD problem in square domain 
%   unsteady_navier          - unsteady flow problem in square domain
%   unsteady_obstacle_navier - unsteady flow problem in obstacle domain
%   unsteady_step_navier     - solve Navier-Stokes problem in step domain
%   unsteadyflowbc           - imposes inflow boundary condition 
%   xxheatbc                 - hot wall Dirichlet boundary condition 
%   colamdAxB                - refined saddlepoint system solver      
%   defaultAxB               - standard MATLAB saddlepoint system solver      
%   step_heat                - solve heat equation in backward step domain 
%   xxstreambc               - dynamic Dirichlet BC on streamfunction
%   restart_step_navier      - restart unsteady flow in step domain
%   unsteady_channel_navier  - solve Navier-Stokes problem in channel domain
%   energymeanvorticity      - compute acceleration and mean vorticity
%   restart_cavity_navier    - restart unsteady flow in cavity domain
%   restart_channel_navier   - restart unsteady flow in channel domain
%   restart_obstacle_navier  - restart unsteady flow in obstacle domain
%   restart_stabtrNS         - restarted N-S integrator using stabilized TR
