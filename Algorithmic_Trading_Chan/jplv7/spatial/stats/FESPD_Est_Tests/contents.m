% Fixed effects Spatial panel data estimation using the methodology 
% developed by Lee and Yu (Journal of Econometrics, 2009, 154, 165-185)
% and spatial autocorrelation tests.
% 
%           f_sac_panel : Computes concentrated likelihood function for SAC model         
%            f_sarpanel : Computes concentrated likelihood function for SAR model        
%            f_sempanel : Computes concentrated likelihood function for SEM model         
%          f2_sac_panel : Computes final value of Likelihood function for SAC model       
%           f2_sarpanel : Computes final value of Likelihood function for SAC model
%           f2_sempanel : Computes final value of Likelihood function for SAC model
%              lm_f_err : LM test for spatial autocorrelated error term
%            lm_f_err_c : LM test for spatial autocorrelated error term when SAR is already accounted for
%            lm_f_joint : Joint LM test for the presence of spatial autocorrelation
%              lm_f_sar : LM test for SAR specification
%            lm_f_sar_c : LM test for SAR when spatially autocorrelated errors are already accounted for
%              lr_f_err : LR test for the presence of spatially autocorrelated errors
%            lr_f_err_c : LR test for spatial autocorrelated error term when SAR is already accounted for 
%            lr_f_joint : Joint LR test for the presence of spatial autocorrelation
%              lr_f_sar : LR test for SAR specification
%            lr_f_sar_c : LR test for SAR when spatially autocorrelated errors are already accounted for
%     sarar_panel_FE_LY : Computes fixed effects SAR panel data model estimates
%       sar_panel_FE_LY : Computes fixed effects SARAR panel data model estimates
%       sem_panel_FE_LY : Computes fixed effects SEM panel data model estimates
%           trans_tslow : Transforms the data structure from i being the slow moving index and t the fast moving index to i being the fast moving
%                         index and t the slow moving index.
% 
%                  demo : Demonstration file using the mat81q spatial weight matrix (queen contiguity)
%                  
