%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
        function [ut,nbc]=MRA_tri1(u,seuil) ;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   function  [ut,nbc]=MRA_tri1(u,seuil)  
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%   MRA: Multi Resolution Analysis
%%   Get rid of small signal coefficients  | u(i) | < seuil
%%   
%%   Input  : u     1D original signal
%%            seuil threshold value
%%
%%   Output : ut    modified signal 
%%            nbc   significant coefficients number   
%%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
        n=size(u,2);
        ut=zeros(1,n);
        nbc=0;
        for i=1:n
            if (abs(u(i))>seuil)
                nbc=nbc+1;
                ut(i)=u(i);
            end
        end        
