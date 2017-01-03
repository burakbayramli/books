function [] = plot_struct(xi,yi);

%plot(xi,yi);
l(1) = line([xi(1) xi(2)], [yi(1), yi(2)]);
l(2) = line([xi(2) xi(3)], [yi(2), yi(3)]);
l(3) = line([xi(3) xi(4)], [yi(3), yi(4)]);
l(4) = line([xi(4) xi(1)], [yi(4), yi(1)]);
l(5) = line([xi(5) xi(6)], [yi(5), yi(6)]);
l(6) = line([xi(6) xi(7)], [yi(6), yi(7)]);
l(7) = line([xi(7) xi(8)], [yi(7), yi(8)]);
l(8) = line([xi(8) xi(5)], [yi(8), yi(5)]);
l(9) = line([xi(1) xi(5)], [yi(1), yi(5)]);
l(10) = line([xi(2) xi(6)], [yi(2), yi(6)]);
l(11) = line([xi(3) xi(7)], [yi(3), yi(7)]);
l(12) = line([xi(4) xi(8)], [yi(4), yi(8)]);
l(13) = line([xi(4) xi(9)], [yi(4), yi(9)]);
l(14) = line([xi(3) xi(10)], [yi(3), yi(10)]);
l(15) = line([xi(8) xi(11)], [yi(8), yi(11)]);
l(16) = line([xi(7) xi(12)], [yi(7), yi(12)]);
l(17) = line([xi(11) xi(12)], [yi(11), yi(12)]);
l(18) = line([xi(12) xi(10)], [yi(12), yi(10)]);
l(19) = line([xi(9) xi(10)], [yi(9), yi(10)]);
l(20) = line([xi(9) xi(11)], [yi(9), yi(11)]);
%set(l(9),'Color','red');
%set(l(10),'Color','red');
%set(l(11),'Color','green');
%set(l(12),'Color','green');
%set(l(18),'Color','yellow');
%set(l(20),'Color','yellow');



