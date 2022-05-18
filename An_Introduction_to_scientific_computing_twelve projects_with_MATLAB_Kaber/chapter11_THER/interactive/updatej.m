%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%======================
% adds a diary line
%======================
function updatej(texto)

global stj txtD xyD DiaryYM ncarl

ntot=length(texto);  % total number of characters

for iligne=1:floor(ntot/ncarl)+1 % add lines

sj=get(stj,'String'); % old diary

txtSep=1.25*txtD;

if  xyD(4)+txtSep > DiaryYM
    % not enough room for a new line
    sjn=sj(2:size(sj,1),:); % eliminate the first line
else
    % we can add new line
    xyD(2)=xyD(2)-txtSep;
    xyD(4)=xyD(4)+txtSep;
	sjn=sj;
end

% split the lines without separating in words! to be reviewed 
ideb=1+ncarl*(iligne-1);
ifin=min(ncarl*iligne,ntot);

for k=ideb:ifin
   set(stj,'Position',xyD);
   set(stj,'String',char(sjn,texto(ideb:k)));pause(0.01);
end

end

