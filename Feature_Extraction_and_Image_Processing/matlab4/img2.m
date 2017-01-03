clear
read_pic = imread('tt1.bmp'); 
emil_colormap;  				% user-defined greyscale colormap 
colormap(emil); 				% set colormap
pic = double(read_pic);		% convert from uint8  

[rows,cols] = size(pic);

newpic_Mx = zeros(rows,cols);
newpic_My = zeros(rows,cols);
pic_xy = zeros(rows,cols);

rows
cols
%horizontal edge detector operator - this detects vertical edges
for x_pos = 2:(cols - 1)		% traverse across the image's matrix
   
   for y_pos = 2:(rows -1)		%traverse down the image matrix
      
   temp = (1 * pic((y_pos - 1),(x_pos - 1)) + 2 * pic((y_pos), (x_pos - 1)) + 1 * pic((y_pos+1),(x_pos-1)) -1 * pic((y_pos-1),(x_pos +1)) -2*pic(y_pos,(x_pos+1)) -1*pic(y_pos+1,x_pos+1));
   	%application of sobel to detect vertical edges
   
   	%if temp > 255
      %	newpic_Mx(y_pos - 1, x_pos - 1)  = 255; 
   	%else
         newpic_Mx(y_pos - 1 , x_pos -1)  = temp;
         
   %	end
   end
end

newpic_Mx =newpic_Mx(1:rows - 2,:);
newpic_Mx =newpic_Mx(:,1:cols - 2);

imwrite(abs(newpic_Mx),emil,'vert_edges_detected_Mx.bmp'); % write out image to disk

%vertical edge detection operator - this detects horizontal edges
for y_pos = 2:(rows - 1)		% traverse across the image's matrix

   
   for x_pos = 2:(cols -1)		%traverse down the image matrix 
      
   temp = (1 * pic((y_pos - 1),(x_pos - 1)) + 2 * pic((y_pos - 1), (x_pos)) + 1 * pic((y_pos-1),(x_pos+1)) -1 * pic((y_pos+1),(x_pos -1)) -2*pic((y_pos+1),(x_pos)) -1*pic(y_pos+1,x_pos+1));
   %application of sobel to detect horizontal edges
   
   %	if temp > 255
      %	newpic_My(y_pos - 1, x_pos - 1)  = 255;
      
   %	else
     	newpic_My(y_pos - 1 , x_pos -1) = temp;
  % 	end
   
   end
end

newpic_My =newpic_My(1:rows - 2,:);
newpic_My =newpic_My(:,1:cols - 2);

imwrite(abs(newpic_My),emil,'hz_edges_detected_My.bmp');

%magnitude of horizontal and vertical detected edges
for x = 1: cols - 2
   
   for y =1:rows  -2 
      
      pic_xy(y,x) = floor(((newpic_Mx(y,x)*newpic_Mx(y,x)) + (newpic_My(y,x)*newpic_My(y,x)))^0.5);
      
         
   end
   
end

pic_xy = pic_xy(1:rows - 2,:);
pic_xy = pic_xy(:,1:cols - 2);


imwrite(pic_xy,emil,'sobelised.bmp');
wk1write('newpic_vertical_edges_detected_Mx',newpic_Mx);
wk1write('newpic_horizontal_edges_My',	newpic_My);
wk1write('newpic_sobelised',pic_xy);


%edge direction of horizontal and vertical detected edges

for x = 1: cols -2   
   	for y =1:rows -2 
                  
      		if(newpic_Mx(y,x) == 0)
      
      				if (newpic_My(y,x) == 0)
                  edge_dir(y,x) = 0;
                  
               	elseif(newpic_My(y,x) > 0)
                  edge_dir(y,x) = (90*pi/180);
                  
                  
         			elseif(newpic_My(y,x) < 0)
                  edge_dir(y,x) = (270*pi/180);
                  
               	end
                       
            else edge_dir(y,x) = atan(newpic_My(y,x)/newpic_Mx(y,x));
               
            end
            
            
                    
      

                        
    	    if(newpic_Mx(y,x) > 0 & newpic_My(y,x) == 0) 		
              edge_dir(y,x) = (360*(pi/180));
              
        		elseif(newpic_Mx(y,x) < 0 & newpic_My(y,x) == 0) 			
                 edge_dir(y,x) = (180*(pi/180));
                 
              elseif(newpic_Mx(y,x) > 0 & newpic_My(y,x) > 0) 			%1
           			edge_dir(y,x) = (edge_dir(y,x) );

                 
              elseif(newpic_Mx(y,x) < 0 & newpic_My(y,x) < 0)					%2
                 edge_dir(y,x) =  ((180*(pi/180)) + edge_dir(y,x));
            
   	    	elseif(newpic_Mx(y,x) < 0 & newpic_My(y,x) > 0)					%3
      	    		edge_dir(y,x) =   ((180*(pi/180)) + edge_dir(y,x));
       
         	elseif(newpic_Mx(y,x) > 0 & newpic_My(y,x) < 0)					%4
            		edge_dir(y,x) = ((360*(pi/180)) + edge_dir(y,x));
         
    		end
   
      end

   end
   


edge_dir;
wk1write('edge_direction_tt1',edge_dir);


intens_map = log10(1 + pic_xy);



