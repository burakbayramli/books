% Contour extraction form a binary image

function outputcontour = Contour(inputimage)

	%Image size
	[rows,columns]=size(inputimage);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	% num neighbours #Black~=8
	border=zeros(rows,columns);
	for x=2:columns-1
	  for y=2:rows-1
	    if inputimage(y,x)==0
	       b=0;
	       for Nx=x-1:x+1
	         for Ny=y-1:y+1
	             if(x~=Nx | y~=Ny)
	               if inputimage(Ny,Nx)==0
	                  b=b+1;
	               end
	             end
	          end
	       end     
	       if(b~=8) border(y,x)=1; end
	    end
	  end
  end   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	  
	   
	   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%Thin borders
%
%      delete pixel if does not break a chain
%              N8: neighbours !=0
%
%      (x,y):
%               N=Card(N8(x,y))
%								B=Sum(Card(N8(P))  for P in N8(x,y)
%
%              if((n-1)*2==B) not break a chain
%

	for x=2:columns-1
      for y=2:rows-1
          
         if border(y,x)>0
         	N=0; B=0;  % num neighbours 
         
         	for Nx=x-1:x+1  % 8 Neigbour
      			for Ny=y-1:y+1
         			if ((Nx~=x | Ny~=y) & border(Ny,Nx)>0)
         			  N=N+1;
         			
         				for NNx=x-1:x+1  % 8 Neigbour
      						for NNy=y-1:y+1
      							if ((NNx~=x | NNy~=y) & border(NNy,NNx)>0)
      						    if ((NNx~=Nx | NNy~=Ny))
      						      if( abs(NNx-Nx)<2 & abs(NNy-Ny)<2)
      						        B=B+1;          
         			          end 
        						  end
         					  end	
         				  end
         				end
         			
        			end
         		end	
         	end
         	
          if((N-1)*2==B)
         	    border(y,x)=0;
         	end 
         	
         end
      end
  end           
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% follow   
	outputcontour=[];  
	   
	%Search starting point
	dmin=rows+columns;
	strtx=0; strty=0; 
	for x=2:columns-1
	  for y=2:rows-1
	  if inputimage(y,x)==0
    d=y+x;
     if(d<dmin)
	       dmin=d;
       strtx=x; strty=y;
	    end
	  end
	  end
	end  
  
  %insert initial point
  sx=strtx; sy=strty;
  outputcontour=[outputcontour [sx;sy]];
  border(sy,sx)=0; %point in the output contour
 
  % next point
  for x=sx-1:sx+1;
     for y=sy-1:sy+1
        if(border(y,x)~=0)
          cx=x; cy=y;
        end
     end
  end
    
  % border following
  while( (cx~=strtx | cy~=strty) )
     outputcontour=[outputcontour [cx;cy]]; %store current point 
     border(sy,sx)=0; %point in the output contour
     sx=cx; sy=cy;
     
     % next point
     n=size(outputcontour,2); % num pts
     stp=0;
  	 for x=sx-1:sx+1
       for y=sy-1:sy+1  
         if( (x~=sx | y~=sy) & ~stp) 
          if(n>3 & x==strtx & y==strty) % arrive to the en
            cx=x; cy=y;
            stp=1;             % stop cicle
          elseif(border(y,x)~=0)
            cx=x; cy=y;
          end
         end 
       end 
     end    
       
  end %while  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph of the curve 
  X=outputcontour(1,:);
  Y=outputcontour(2,:);

 	plot(X,Y);   
 	mx=max(max(X),max(Y))+10;                
 	axis([0,mx,0,mx]); % Axis of the graph pf the curve
 	axis square;                     % Aspect ratio
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
