% GRAPHS
%
% Files
%   bookplot                  - saves IFISS figure as postscript file
%   box_heatplot              - evolves temperature data on cavity domain
%   dttplot                   - plots timestep data 
%   ell_heatplot              - evolves temperature data on L-shaped domain
%   ellx                      - outlines L-shaped domain
%   eplot                     - plots Q1 element data on arbitrary domain
%   eplotl                    - plots Q1 element data on L-shaped domain
%   errplot                   - plots solution and error on square domain
%   errplotl                  - plots solution and error on L-shaped domain
%   exportfig                 - Export a figure to Encapsulated Postscript.
%   flowplot09                - plots flow data on general domain
%   flowplotl                 - plots flow data on standard step domain
%   flowplotp                 - plots flow data on slit shaped domain
%   flowplotz                 - plots flow data on extended step domain
%   flowvolume                - plots flow solution on vertical X-section 
%   htmlplot                  - saves IFISS figure as html file
%   logoplot                  - plots IFISS logo
%   mplot                     - plots 2x2 macro data on square domain
%   mplotl                    - plots 2x2 macro data on L-shaped domain
%   obstacle_flowmovie        - movie of flow around square obstacle
%   obstacle_unsteadyflowplot - plots flow data in obstructed channel 
%   obstacle_vorticityplot    - plots vorticity data in obstructed channel
%   outflow                   - plots/explores tangential flow at outflow
%   solplot                   - plots nodal data on square-shaped domain
%   solplotl                  - plots nodal data on L-shaped domain
%   solsurf                   - plots solution surface on square domain
%   solsurfl                  - plots solution surface on L-shaped domain
%   square_flowmovie          - generates flow movie on square domain
%   square_heatplot           - evolves temperature data on square domain
%   square_pressureplot       - compares pressure data on square domain
%   square_unsteadyflowplot   - plots flow data on square domain
%   square_vorticityplot      - plots vorticity data on square domain
%   squarex                   - outlines square shaped domain
%   step_flowmovie            - flow movie on extended step domain
%   step_unsteadyflowplot     - evolves flow data on symmetric/standard step
%   step_vorticityplot        - plots vorticity data on step domain
%   stepx                     - outlines step domain
%   wwwplotobs                - streamlines and error on general domain
%   xrefplot                  - comparison plot of iterative residuals 
%   bouss_solplot             - plots solution evolution on general domain
%   box_unsteadyflowref       - plots cavity flow data at snapshot times 
%   boxx                      - outlines L:H cavity domain in hot/cold 
%   boxz                      - outlines L:H cavity domain in monochrome
%   flowplot12                - flow and element error on general domain
%   snapplot_boussdata        - plots snapshot Boussinesq flow solution
%   square_unsteadyflowref    - plots square domain data at snapshot times
%   step_bdryvorticity        - plots vorticity solution on step boundary
%   step_heatplot             - evolves temperature data on step domain
%   step_unsteadyflowref      - plots step flow data at snapshot times
%   step_unsteadytempref      - plots temperature data at snapshot times
%   bookcoverplot              - generates plot for ESW2 book cover
%   box_unsteadytempref        - plots cavity flow data at snapshot times 
%   eplot2                     - plots Q2 element data on square-shaped domain
%   eplotl2                    - plots Q2 element data on L-shaped domain
%   errplot2                   - plots Q2 solution and error on square domain
%   errplotl2                  - plots Q2 solution and error on L-shaped domain
%   flowxsection               - plots flow solution on horizontal X-section
%   solplot_poissoncontrol     - plots state and control on square-shaped domain
%   solplot_poissoncontrol_ell - plots state and control on L-shaped domain
%   xsectionplot               - plots scalar solution on horizontal X-section 
%   ysectionplot               - plots scalar solution on vertical X-section
%   channel_unsteadyflowref    - plots channel flow data at snapshot times
%   chanx                      - outlines channel domain
%   meanvorticityplot          - plots mean vorticity (in channel flow)
%   obstacle_unsteadyflowref   - plots obstacle flow data at snapshot times
%   step_profiles              - generates flow profiles for symmetric step domain
%   stepsym                    - outlines symmetric step domain
%   symstep_flowmovie          - movie of flow around square obstacle
%   symstep_unsteadyflowref    - plots symmetric step flow data at snapshot times
%   shgx                       - switch colormap for enhanced visual display
