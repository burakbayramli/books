% GRIDS
%
% Files
%   box_domain          - box domain Q2 grid generator
%   cavity_domain       - square cavity Q2 grid generator
%   channel_domain      - standard square shaped domain Q2 grid generator
%   edgegen             - edge information for flux jump computation
%   eexgen              - Q2-P1 reorientation for flux jump computation
%   ell_domain          - L-shape domain Q2 grid generator
%   findboundary        - generates vector bound and matrix mbound
%   findobsXY           - finds coords of grid points inside obstacle 
%   fint                - subdivision function 
%   fitint              - computes contraction/expansion ratio
%   forwardstep_domain  - forward step domain Q2 grid generator
%   gridplot            - quadrilateral grid verification
%   longstep_domain     - legacy code | replaced by newstep_domain
%   macrogridplot       - quadrilateral macroelement grid verification
%   obstacle_domain     - obstacle domain Q2 grid generator
%   perturb_grid        - perturbed bilinear element grid generator
%   plate_domain        - slit shaped domain Q2 grid generator
%   q1grid              - bilinear element grid generator
%   q1p0grid            - Q1-P0 element grid generator
%   q1q1grid            - Q1-Q1 element grid generator
%   q2grid              - biquadratic element grid generator 
%   q2p1grid            - Q2-P1 element grid generator
%   q2q1gridx           - Q2-Q1 element grid generator
%   quad_domain         - quadrilateral domain Q2 grid generator
%   rec_domain_q2       - generate Q2 subdivision of a rectangular domain 
%   ref_domain          - reference square domain Q2 grid generator
%   shish_grid          - Shishkin grid generator on unit domain
%   square_domain       - square domain Q2 grid generator
%   step_domain         - legacy code | replaced by newstep_domain
%   stretch_grid        - generates stretched grid
%   subint              - geometrically stretched subdivision generator 
%   subint_contract     - geometrically stretched grid contraction
%   subint_expand       - geometrically stretched grid expansion 
%   bouss_boxdomain     - rectangular cavity grid generator for Boussinesq
%   bouss_stepdomain    - step domain grid generator for Boussinesq
%   grid_mergeleftright - merger of inlet/outlet channel domain 
%   grid_yblock         - vertical domain Q2 grid generator
%   grid_xblock         - horizontal domain Q2 grid generator
%   newstep_domain      - backward-facing step domain Q2 grid generator
%   q2q1q2grid          - Q2-Q1-Q2 element grid generator 
%   ell_domain_x        - pde_control | alternative L-shape grid generator 
%   square_domain_x     - pde_control | alternative square grid generator 
%   newchannel_domain   - rectangular shaped domain Q2 grid generator
%   rsquare_domain      - reference square domain Q2 grid generator
%   twostep_domain      - backward-facing double step domain Q2 grid generator
%   usquare_domain      - (0,1)x(0,1) square domain Q2 grid generator
