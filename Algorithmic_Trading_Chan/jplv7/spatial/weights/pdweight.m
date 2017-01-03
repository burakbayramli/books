function g=pdweight(xcoord,ycoord,lower,upper,RowStdOpt);
% PURPOSE: uses x,y coordinates to produce distance-based spatial weight matrices
%          The user is asked to input x and y coordinates, as well as a lower and upper
%          bound cutoff for the neighborhood.
% ------------------------------------------------------
% USAGE: W = pdweight(xcoord,ycoord,lower,upper,rowstdopt)
% where:     xcoord = x-direction coordinate
%            ycoord = y-direction coordinate
%            lower  = lower bound distance cut-off
%            uppper = upper bound distance cut-off
%            rowstdopt = 1 for row-standardization
%                      = 0 for no standardization
% ------------------------------------------------------
% RETURNS: 
%          W = a sparse weight matrix based on distance cut-offs
%              set by lower and upper input options
% ------------------------------------------------------

% written by: Shawn Bucholtz
% SBUCHOLTZ@ers.usda.gov
% USDA-ERS-ISD-ADB

%This function builds a sparse spatial weight matrix.
%The user is asked to input x and y coordinates, as well as a lower and upper
%bound cutoff for neighborhood.
%The user is given the option of indicating if they would like
%to Row-standarize the weight matrix (1=Row Standardized, 0=Not Row-Standardized)


%Declare inital values;
i=1;
p2=[1 1 -8888];
%Begin loop for each observation;

for i=1:(length(xcoord));
      j=1;
      p1=[1 1 -9999];%Make an inital value;
        %Begin loop to compute distance from observation i to all other observations;
      
            for j=1:(length(xcoord));

                if xcoord(i)==xcoord(j) & ycoord(i)==ycoord(j);
                    D=1;%This ensures that two places with the same x,y location will
                        %be defaulted to 1 distance unit away from eachother.
                        %This can happen in the case of two condos who are assocaited with one building location.
                else;
                    Xdist=abs(xcoord(i)-xcoord(j));
                    Ydist=abs(ycoord(i)-ycoord(j));
                    D = sqrt(Xdist^2 + Ydist^2);
                end;
                
                p = [i j D];
                                                    
                %A loop to generate a list of neighbors j of current obs i
                    if D > lower & D <= upper;%Check to see if j meets the neighborhood cutoff criteria
                        
                        if p1(3) == -9999;  %If this is the first j in the list of neighbors, then
                                            %make it the inital data set;
                            p1=p;
                        else;
                            p1 = [p1;p];%If this is not the first j in the list of neighbors, then append this
                                        %to previous list of all neighbors j for observation i.
                        end;
                        
                    else;
                        p1=p1;%If distance between i and j did not meet neighborhood cutoff criteria, then
                                %do not append this to previous list of neighbors for obs. i;
                    end;
                j=j+1; %Step to next neighbor j of observation i
                    
            end;
            
            if p1(3) == 0;%A Check to see if observation i had any neighbors within cutoff
                i=i+1;%If it did not, then pass loop to next i
            else;
                    
                
                    %If observation i had at least 1 neighbor within cutoff, then append that data to
                    %previous data set for i-1
                    
                    if RowStdOpt == 1;%Row standardize the weight matrix
                        p1(:,3) = p1(:,3) ./ sum(p1(:,3));
                    end;
                    if p2(3) == -8888;%If the current observation i is the first observationervation to have any neighbors
                                        %then make it the first data set;
                        p2=p1;
                    else;
                        p2=[p2;p1];%If the current observation i is not the first observation to have any neighbors
                                    %then append current observation i's neighbors to all other previous i's neighbors
                        clear p1;
                    end;
                
                 i=i+1;%pass loop to next i
            end;
  end;
  
  %Generate the sparse matrix
  g=sparse(p2(:,1),p2(:,2),p2(:,3));