%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%=======================================================
% Reads the mesh and all the physical parameters
%=======================================================

% identifies the type of the mesh file
Fext=Nomfic(min(find(Nomfic=='.'))+1:length(Nomfic));
if(Fext=='msh')
                       updatej('FreeFem++ mesh file')
Read_mail_freefem;     % reads the mesh
elseif(Fext=='amb')
                       updatej('EMC2 mesh file')
Read_mail;            % reads the mesh
else
                        updatej('************unknown mesh file')                      
end
                        updatej('Reading mesh... OK');
                        
airet=0;              % computes the area of the mesh
for k=1:Nt;
 airet=airet+Aire(k);
end
								updatej(['Area of the mesh = ' num2str(airet)])
                                updatej('The mesh is now displayed...')
                                updatej(' ***********************')

Visu;                            % Creation of the visualization window      
[Ireft,CIreft]=Show_mail(fg2);   % Mesh visualization
                                updatej(' ***********************')

								updatej('The internal boundaries are now displayed...')
[xb,yb]=Find_border(Ireft);
hold on;Show_border(xb,yb,fg2);hold off;
								updatej('The labels of the boundaries are now displayed...')
                                updatej(' ***********************')
                                [Irefd,CIrefd]=Show_ref(fg2);    % Identification of the labels

VisuB;                          % Choice of the physical values
                                updatej(' ***********************')
