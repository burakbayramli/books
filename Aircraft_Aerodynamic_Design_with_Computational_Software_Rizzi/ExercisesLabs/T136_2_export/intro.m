function []=intro(void)
disp('******************************************************');
disp('*                                                    *');
disp('*              Welcome to Tornado 136.001            *');
disp('*                                                    *');
disp('*                  by: Tomas Melin                   *');
disp('*                Copyright 1999-2016                 *');
disp('******************************************************');
disp('*                                                    *')
disp('* Release 136.001   -  BETA RELEASE - 2016 12 02     *')
disp('* Changes since last revision:                       *')
disp('*                                                    *')
disp('* Rudder moment computation added in postprocessor   *')
disp('* Error in ruddersweep fixed                         *')
disp('* Menus changed                                      *')
disp('* Bending moment computation correcected             *')
disp('* Strip theory modified                              *')
disp('* General code cleanup                               *')
disp('* Menus changed                                      *')
disp('* Unsteady solver added                              *')
disp('* Strip theory added                                 *')   
disp('* Unsteady solver added                              *')
disp('* Find static margin function added                  *')
disp('* Falling back to slope2 in fcreate lattice          *')
disp('* Move cg to ac def                                  *')
disp('* Put allmoving in                                   *')
disp('* Move apex to wing def in inpt19                    *')
disp('* Reshuffeling input order                           *')
disp('* More input data sanity checks                      *')
disp('* Added geometry name to geo in inpt19               *')
disp('* Added project reference to geo in inpt19           *')
disp('* Added allmoving surface to geo in inpt19           *') 
disp('* Added geometry Version to geo in inpt19            *')
disp('* Bodyinput.m moved inline with inpt19.              *')
disp('* Changed text in rudder deflection to show abolute  *') 
disp('*  setting.                                          *')
disp('* Fixed geometry plotting from geometry edit menu    *')
disp('* Optional Update of old geometry files added.       *')

%
disp('* Release 135.005      2008-11-17                    *')
disp('* Release 135.004      2008-09-26                    *')
disp('* Work in progress release                           *')
disp('* DO NOT DISTRIBUTE                                  *')
disp('*                                                    *')
disp('* Release 135.003      2008-04-28                    *')
disp('* Work in progress release                           *')
disp('* DO NOT DISTRIBUTE                                  *')

disp('*                                                    *')
disp('* Release 135.002      2008-03-20                    *')
disp('* Work in progress release                           *')
disp('* DO NOT DISTRIBUTE                                  *')

disp('*                                                    *')
disp('* Release 134 Beta      2007-11-15                   *')
disp('* Change directory command changed to functional form*')
disp('* Variable directory names added in config.m         *')
disp('* Aircraft trim function added                       *')

disp('*                                                    *')
disp('* Release 133, never released to public (2007-11-14) *')
disp('* New solver added, vectorised 6 times faster        *')
disp('* Symmetric solver removed                           *')
disp('* Simple solution now computes derivatives as well   *')
disp('* Zero lift drag prediction added                    *')
disp('* Definition plots added                             *')
disp('* Result data exporter added                         *')
disp('*                                                    *')


disp('* Release 1.32      2007-08-02                       *')
disp('* Rotation in boundary condition error fixed         *')
disp('* New Camber model added, more accurate than before  *')
disp('* Viscosity added to ISA                             *')
disp('* Subfunctions moved inline                          *')
disp('* Symmetric solver added.                            *')
disp('* Editor fixed to vork with new foil standad         *')
disp('* Batch function added.                              *')
disp('*                                                    *')

disp('* Release 1.31 Beta 2007-02-06                       *')
disp('* Trefftz plane analysis added                       *')
disp('* More airfoils added to library                     *')
disp('* Geo.foil chage type from double to cell            *')
disp('* Splash screen added                                *')
disp('* The way that geometry18 calls slope2 is changed    *')
disp('*                                                    *')

disp('* Release 1.30 Beta 2005-12-04                       *')
disp('* Wind alignment modified                            *')
disp('*                                                    *')

disp('* Release 1.29 Beta 2005-11-04                       *')
disp('* Wake alignment fixed                               *')
disp('* Normal position on panel fixed                     *')
disp('* Wind to body transform modified                    *')
disp('* Sideslip computation modified                      *')
disp('* Standard (Horseshoe) VL method added               *')
disp('*                                                    *')

disp('* Release 1.28 Beta 2004-11-04                       *')
disp('* Rudder setting problem fixed                       *')


disp('* Release 1.27 Beta 2004-07-20                       *')
disp('*  Mop up job after memory structure change.         *')

disp('* Release 1.26 Beta 2004-03-07                       *')
disp('*  Memory structure changed                          *')
disp('*  geometry, state, lattice and result variables     *')
disp('*  moved to structures                               *')
disp('*  major changes in all functions                    *')


disp('*  Old gromertry files obsolete                      *')
disp('*  Geometry file conversion function added           *')


disp('*                                                    *')
disp('* Release 1.25 Beta 2004-01-21                       *')
disp('*  Not distributed                                   *')
disp('*                                                    *')


disp('* Release 1.24 Beta 2003-08-27                       *')
disp('*  More spell-check                                  *')
disp('*  result plotting functions made as one fcn         *')
disp('*  Sideforce computation corrected                   *')
disp('*  flapped_vector format corrected                   *')
disp('*                                                    *')


disp('* Release 1.23 Beta 2002-10-09                       *')
disp('*  Reference point position changed to geometry      *')
disp('*  property.                                         *')
disp('*  Downwash matrix condition computed in solver6     *')
disp('*  Input function shanged to accommodate new variable*')
disp('*  Diffbatch changed to accommodate new variables.   *')
disp('*  fOld2New function created to translate old        *')
disp('*  geometry files into new.                          *')
disp('*  fLattice_setup created. Its a Wrapper for         *')
disp('*  geosetup and wake stetup etc.                     *')
disp('*                                                    *')
disp('*                                                    *')

disp('* Release 1.22 Beta 2002-10-01                       *')
disp('*  Rudder setting changed to geometry property       *')
disp('*  Current rudder setting displayed in result plot   *')
disp('*  Input error for profile taper fixed               *')
disp('*  continious camber error fixed                     *')
disp('*  Note: New variable in geometry file: flap_setting *')
disp('*                                                    *')
disp('*                                                    *')
disp('* Release 1.21 Beta 2002-08-07                       *')
disp('*  Comments added,                                   *')
disp('*  Code cleanup                                      *')
disp('*                                                    *')

disp('* Release 1.20 Beta 2002-02-10                       *')
disp('*  Fixes since last revision                         *')
disp('*  New Cmac computation added                        *')
disp('*  Grapical C_mac and refpoint added                 *')
disp('*  Spanload computation and graph added              *')
disp('*  Beta sign inconsistency corrected                 *')
disp('*  Wake roll inconsistency corrected                 *')
disp('*  Wake length edited.					           *')
disp('*  NACA profile slope error corrected                *')
disp('*  Refpoint move to MAC added                        *')
disp('*  Input questions fixed for flapped assymetric wings*')
disp('*  Mean geometric chord added (not used yet)         *')
disp('*                                                    *')

disp('* Release 1.19 Beta 2001-11-07                       *')
disp('*  Fixes since last revision                         *')
disp('*  Geometry plot sped up                             *')
disp('*                                                    *')


disp('* Release 1.18 Beta 2001-11-05                       *')
disp('*  Fixes since last revision                         *')
disp('*  rudder sweep misarrayment fixed                   *')
disp('*  Main function picture removed                     *')


disp('*                                                    *')
disp('* Release 1.17 Beta 2001-10-18                       *')
disp('*  Fixes since last revision                         *')
disp('*  Hotfix for matlab6                                *')
disp('*                                                    *')
disp('* Release 1.16 Beta 2001-10-12                       *')
disp('*  Fixes since last revision                         *')
disp('*  Plotting functions enabeled for old jobs.         *')
disp('*  Job IDentifiers (JID) added.                      *')
disp('*  More spellcheck                                   *')
disp('*  Vorticity plot removed.                           *')
disp('*  Output files with variable names enabeled.        *')
disp('*                                                    *')
disp('* Release 1.15 Beta 2001-10-11                       *')
disp('*  Fixes since last revision                         *')
disp('*  State input stabilised.                           *')
disp('*  State save change according to geometry.          *')
disp('*                                                    *')
disp('* Release 1.14 Beta 2001-10-10                       *')
disp('*  Fixes since last revision                         *')
disp('*  Geomtry input stabilised                          *')
disp('*  Meshing added as separate function.               *')
disp('*  Main menu changed                                 *')
disp('*  Minor changes in output plots.                    *')
disp('*                                                    *')
disp('* Release 1.13 Alpha 2001-01-15                      *')
disp('*  Fixes since last revision                         *')
disp('*  Changes in solver 4, to handle matrices better.   *')
disp('*  Solver loop, extra input variable to handle case  *')
disp('*  calls.                                            *')
disp('*  Diffbatch, now works through solverloop.          *')
disp('*                                                    *')
disp('* Release 1.13 Beta 2000-12-5                        *')
disp('*  Fixes since last revision                         *')
disp('*	 Error in sideslip BC corrected                   *')
disp('*   Error in solverloop, variable resetting -        *')
disp('*   corrected                                        *')
disp('*   S_ref & panel Area now computed for first wing   *')
disp('*                                                    *')
disp('* Release 1.12 Beta 2000-10-22                       *')
disp('*  Fixes since last revision                         *')
disp('*	 minor variable name changes                      *')                                                    
disp('*                                                    *')
disp('* Release 1.11 Beta 2000-10-10                       *')
disp('*  Fixes since last revision                         *')
disp('*	 in inpt11.m: interface cleaned up                *')                                                    
disp('*   in several functions: Spelcheck ;-)              *')                                                  
disp('*                                                    *')                                                     
disp('*                                                    *')
disp('* Release 1.10 Beta 2000-10-02                       *')
disp('*  Fixes since last revision                         *')
disp('*	divisions change name to partitions for better    *')                                                    
disp('*  clearity.                                         *')                                                  
disp('*  Bug in setrudder fixed for multiple wing AC       *')                                                     
disp('*                                                    *')
disp('*                                                    *')
disp('* Release 1.09 Beta 2000-09-17                       *')
disp('*                                                    *')
disp('*  Fixes since last revision                         *')		                                                  
disp('*     Directorys decapitated Unix users.             *')
disp('*     interface cleaned up/stabilized                *')                                                
disp('*                                                    *')
disp('*                                                    *')
disp('* Release 1.08 Beta 2000-08-10                       *')
disp('*                                                    *')
disp('* Fixes since release 1.07                           *')
disp('*  in inpt11                                         *')
disp('*    old wing features deleted before new added      *')
disp('*    -prventing aliasing of older designs            *')
disp('*                                                    *')
disp('*  copyright.m                                       *')
disp('*     copyright information added with this function *')
disp('*                                                    *')
disp('* Release 1.07 Beta 2000-07-28                       *')
disp('*                                                    *')
disp('* Fixes since release 1.06                           *')
disp('*                                                    *')
disp('*  In main                                           *')
disp('*    terror call corrected  (error -> terror)        *')
disp('*    Main menu changed                               *')
disp('*                                                    *')
disp('*  In Wakesetup                                      *')
disp('*    Length of wake moved to config                  *')
disp('*                                                    *')
disp('*  In geoinput                                       *')
disp('*    All property vectors made to the same length    *')
disp('*    -Preventing out of index crash in geosetup      *')
disp('*                                                    *')
disp('*  In tedit                                          *')
disp('*    Remove division made operational                *')
disp('*                                                    *')
disp('* Release 1.06 Beta 2000-06-28                       *')
disp('*                                                    *')
disp('* Fixes since release 1.05                           *')
disp('*                                                    *')
disp('* In Solver                                          *')
disp('*   Induced drag now calculated correctly            *')
disp('*                                                    *')
disp('* New function "config.m"                            *')
disp('*   Edit this function to enable static              *')
disp('*   reference variables, s.a. S_ref, rho, C_mac      *')
disp('*                                                    *')
disp('* In geosetup                                        *')
disp('*    b_ref calculation corrected for symmetric wings *')
disp('*                                                    *')
disp('* In rplot             	                           *')
disp('*    figure 5 edited to show z-component             *')
disp('*    of every panel load                             *')
disp('*                                                    *')
disp('* Fixes since relese 1.04                            *')
disp('*                                                    *')
disp('* In Dnwash                                          *')
disp('*      bug-parasite line erased                      *')
disp('* In rplot                                           *')
disp('*      Handling of b_ref fixed                       *')
disp('*                                                    *')
disp('* Fixes since release 1.03                           *');
disp('*                                                    *')
disp('* In solverloop,                                     *');
disp('*     delta deflection in  [deg] rather than [deg/s] *');
disp('*     Ruddersweep corrected                          *');
disp('*     Title added on sweep plots                     *');
disp('*     Row reduction                                  *');
disp('*     Taylor expansion fixed                         *');
disp('* In MAIN                                            *');
disp('*     b_ref handling fixed according to S_ref        *');
disp('* Specfun                                            *');
disp('*     edited for 3-dimensions and batch              *');
disp('* Taylorbatch                                        *');
disp('*     fixed for new function calls                   *');
disp('*     renamed to diffbatch (more appropriate)        *');
disp('* dnwash                                             *');
disp('*     functions tcros & tnorm sped innermost         *');
disp('*     loop up with 25 percent                        *');
disp('*                                                    *');
disp('*                                                    *');
disp('*                                  Happy Hacking  /T *');
disp('******************************************************');
disp('*                                                    *');
disp('* Tornado, Copyright (C) 2000, Tomas Melin           *');
disp('* Tornado comes with ABSOLUTELY NO WARRANTY;         *');
disp('*                                                    *');
disp('* This is free software,and you are welcome to       *');
disp('* redistribute it under certain conditions;          *');
disp('* see Release Info for details.                      *');                                               
disp('*                                                    *');   
disp('******************************************************');
disp(' ');
end