%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
        function [ut,nbc]=MRA_tri2(u,seuil);                               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function  [ut,nbc]=MRA_tri2(u,seuil)  
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   MRA: Multi Resolution Analysis
%%   Get rid of small signal coefficients  | u(i) | < seuil
%%   
%%   Input  : u     2D original signal
%%            seuil threshold value
%%
%%   Output : ut    modified signal 
%%            nbc   significant coefficients number   
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
        n=size(u,2);
        seuil2=seuil*n*n;
        ut=zeros(n);
        nbc=0;
        for i=1:n
        for j=1:n
            if (abs(u(i,j))>seuil2)
                nbc=nbc+1;
                ut(i,j)=u(i,j);
            end
        end
        end        
