function [WEG1,TANG1,STEP1,errorcode] = ...
         pitcon1(F,WEG,TANG,STEP,Parcont,Parmeter)
% Continuation after Rheinboldt86
errorcode = 0;
% -- Parameter -------------------------
maxit = Parcont(1);
tol   = Parcont(2);
HMIN  = Parcont(3);
% --------------------------------------
iter = 0; done = 0;
while ~done
   iter = iter +1;
   [t,J]= pitcon2(F,WEG,TANG,Parmeter);    % Tangent
   TANG = [TANG,t];
   h    = pitcon3(F,WEG,TANG,J,Parmeter); % Step length
   newtondone = 0;
   while ~newtondone
      [Y,errorcode] = pitcon4(F,WEG,TANG,J,h,Parmeter); % Newton method
      if errorcode == 0
         WEG = [WEG,Y]; newtondone = 1;
      else
         h = h/2;
         if h < HMIN, newtondone = 1; end
      end
   end
   if h < HMIN, disp('Minimum step length '); end
   done = (h < HMIN) | (iter > maxit);
   STEP = [STEP,h];
   ITER_J_H = [iter,J,h]
end
if iter > maxit, errorcode = 1; end
WEG1 = WEG; TANG1 = TANG; STEP1 = STEP;
