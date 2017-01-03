function plt_dfb(results,vnames)
% PURPOSE: plots BKW influential observation diagnostics
%          dfbetas
%---------------------------------------------------
% USAGE: plt_dfb(results,vnames)
% where: results = a structure returned by dfbeta
%         vnames = an optional vector of variable names
%---------------------------------------------------               
%                 e.g. vnames = ['y    ',
%                                'x1   ',  NOTE: fixed width
%                                'x2   ',        like all MATLAB
%                                'cterm'];
% --------------------------------------------------
% RETURNS:
%        nothing, simply plots the dfbetas 
% --------------------------------------------------
% SEE ALSO: dfbeta, plt_dff, bkw, rdiag, diagnose
%---------------------------------------------------
% REFERENCES: Belsley, Kuh, Welsch, 1980 Regression Diagnostics

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if strcmp(results.meth,'dfbeta') ~=1
error('plt_dfb requires a structure from dfbeta');
end; 

nobs = results.nobs;
nvar = results.nvar;

dfbeta = results.dfbeta;

clf;

tt=1:nobs;

switch nvar
case {1, 2, 3, 4}
cnt = 1;
for i=1:nvar;
   subplot(nvar,1,i), plot(tt,dfbeta(:,i));
     title('df betas');
   if nargin < 2
   ylabel(['var ', num2str(cnt)],'Rotation',90);
   else
   ylabel(vnames(i+1,:),'Rotation',90);
   end;
end;

case {5, 6}
cnt = 1;
for i = 1:nvar
         if cnt <= nvar;
           subplot(3,2,cnt),plot(tt,dfbeta(:,cnt));
           if nargin < 2
         ylabel(['var ', num2str(cnt)],'Rotation',90);
         else
         ylabel(vnames(cnt+1,:),'Rotation',90);
         end;
         end;
cnt = cnt+1;
end

case {7, 8}

cnt = 1;
for i = 1:nvar
         if cnt <= nvar;
           subplot(4,2,cnt),plot(tt,dfbeta(:,cnt));
           if nargin < 2
         ylabel(['var ', num2str(cnt)],'Rotation',90);
         else
         ylabel(vnames(cnt+1,:),'Rotation',90);
         end;
         end;
       cnt = cnt+1;
end;

case {9}

cnt = 1;
for i = 1:nvar
         if cnt <= nvar;
           subplot(3,3,cnt),plot(tt,dfbeta(:,cnt));
           if nargin < 2
         ylabel(['var ', num2str(cnt)],'Rotation',90);
         else
         ylabel(vnames(cnt+1,:),'Rotation',90);
         end;
         end;
       cnt = cnt+1;
end;

case {10}

cnt = 1;
for i = 1:nvar
         if cnt <= nvar;
           subplot(5,2,cnt),plot(tt,dfbeta(:,cnt));
           if nargin < 2
         ylabel(['var ', num2str(cnt)],'Rotation',90);
         else
         ylabel(vnames(cnt+1,:),'Rotation',90);
         end;
         end;
       cnt = cnt+1;
end;

case {11, 12}

cnt = 1;
for i = 1:nvar
         if cnt <= nvar;
           subplot(4,3,cnt),plot(tt,dfbeta(:,cnt));
           if nargin < 2
         ylabel(['var ', num2str(cnt)],'Rotation',90);
         else
         ylabel(vnames(cnt+1,:),'Rotation',90);
         end;
         end;
       cnt = cnt+1;
end;

case {13, 14, 15, 16}

cnt = 1;
for i = 1:nvar
         if cnt <= nvar;
           subplot(4,4,cnt),plot(tt,dfbeta(:,cnt));
           if nargin < 2
         ylabel(['var ', num2str(cnt)],'Rotation',90);
         else
         ylabel(vnames(cnt+1,:),'Rotation',90);
         end;
         end;
       cnt = cnt+1;
end;

case {17, 18, 19, 20}

cnt = 1;
for i = 1:nvar
         if cnt <= nvar;
           subplot(5,4,cnt),plot(tt,dfbeta(:,cnt));
           if nargin < 2
         ylabel(['var ', num2str(cnt)],'Rotation',90);
         else
         ylabel(vnames(cnt+1,:),'Rotation',90);
         end;
         end;
       cnt = cnt+1;
end;


otherwise
error('too many variables to plot nicely');

end;
