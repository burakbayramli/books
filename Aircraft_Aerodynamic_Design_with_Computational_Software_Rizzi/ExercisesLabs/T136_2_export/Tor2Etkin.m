function [state,results] = Tor2Etkin(state,results,Etkinopt)

%%% to transfer the body axis from Tornado to Etkin's
if Etkinopt == 0;
    state = state;
    results = results;
else

stateE.AS = state.AS; % m/s
stateE.alpha = state.alpha;
stateE.betha = state.betha;
stateE.P = -state.P;
stateE.Q =  state.Q;
stateE.R = -state.R;
stateE.alpha_dot = state.alpha_dot;
stateE.beta_dot  = state.beta_dot;
stateE.ALT       = state.ALT;
stateE.rho       = state.rho;
stateE.pgcorr    = state.pgcorr;



results.dwcond =  results.dwcond; 
results.F      =  results.F;
results.FORCE  =  results.FORCE;
results.M      =  [results.M(:,1) -results.M(:,2) -results.M(:,3)];
results.MOMENTS=  [ results.MOMENTS(1)
                   -results.MOMENTS(2)
                   -results.MOMENTS(3)];
results.gamma  =  results.gamma;
results.cp     =  results.cp;
results.L      =  results.L;
results.D      =  results.D;
results.C      =  results.C;
results.CX     = -results.CX;
results.CY     =  results.CY;
results.CZ     = -results.CZ;
results.CL     =  results.CL;
results.CD     =  results.CD;
results.CC     =  results.CC;
results.Cl     = -results.Cl;
results.Cm     =  results.Cm;
results.Cn     = -results.Cn;
results.CL_a   =  results.CL_a;
results.CD_a   =  results.CD_a;
results.CC_a   =  results.CC_a;
results.CX_a   = -results.CX_a;
results.CY_a   =  results.CY_a;
results.CZ_a   = -results.CZ_a;
results.Cl_a   = -results.Cl_a;
results.Cm_a   =  results.Cm_a;
results.Cn_a   = -results.Cn_a;
results.CL_b   =  results.CL_b;
results.CD_b   =  results.CD_b;
results.CC_b   =  results.CC_b;
results.CX_b   = -results.CX_b;
results.CY_b   =  results.CY_b;
results.CZ_b   =  results.CZ_b;
results.Cl_b   = -results.Cl_b;
results.Cm_b   =  results.Cm_b;
results.Cn_b   = -results.Cn_b;
results.CL_P   = -results.CL_P;
results.CD_P   = -results.CD_P;
results.CC_P   = -results.CC_P;
results.CX_P   =  results.CX_P;
results.CY_P   = -results.CY_P;
results.CZ_P   =  results.CZ_P;
results.Cl_P   =  results.Cl_P;
results.Cm_P   = -results.Cm_P;
results.Cn_P   =  results.Cn_P;
results.CL_Q   =  results.CL_Q;
results.CD_Q   =  results.CD_Q;
results.CC_Q   =  results.CC_Q;
results.CX_Q   = -results.CX_Q;
results.CY_Q   =  results.CY_Q;
results.CZ_Q   = -results.CZ_Q;
results.Cl_Q   = -results.Cl_Q;
results.Cm_Q   =  results.Cm_Q;
results.Cn_Q   = -results.Cn_Q;
results.CL_R   = -results.CL_R;
results.CD_R   = -results.CD_R;
results.CC_R   = -results.CC_R;
results.CX_R   =  results.CX_R;
results.CY_R   = -results.CY_R;
results.CZ_R   =  results.CZ_R;
results.Cl_R   =  results.Cl_R;
results.Cm_R   = -results.Cm_R;
results.Cn_R   =  results.Cn_R;

state = stateE;
% results.CL_d   =  results.CL_d;
% 
%              Cl_R: 0.1359
%              Cm_R: 0.0012
%              Cn_R: -0.6606
%              CL_d: [-1.4041e-005 0.9120 0.0517 -2.3447e-005 0.0363]
%              CD_d: [2.5417e-005 0.0641 0.0031 2.0445e-005 0.0092]
%              CC_d: [0.0024 -6.4081e-015 -9.0626e-014 -0.1077 1.4625e-014]
%              CX_d: [2.6433e-005 -0.0071 -9.0860e-004 2.2209e-005 0.0064]
%              CY_d: [0.0024 -6.4081e-015 -9.0626e-014 -0.1077 1.4625e-014]
%              CZ_d: [-1.2019e-005 0.9142 0.0518 -2.1784e-005 0.0370]
%              Cl_d: [0.0666 2.0233e-014 3.8570e-015 0.0119 1.3232e-014]
%              Cm_d: [3.7751e-005 -2.4361 -0.1063 8.3362e-005 0.2024]
%              Cn_d: [0.0049 -8.0837e-015 -1.0218e-013 -0.1321 1.0841e-014]
%          ystation: [32x3 double]
%     ForcePerMeter: [32x3 double]
%          CL_local: [32x3 double]
end