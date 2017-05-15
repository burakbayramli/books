% Display gaussian mask
fil=fspecial('gaussian',[20 20],4);
surf(fil);
title('gaussian mask');