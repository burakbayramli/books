function [] = plot3_cube(xi,yi,zi);


%plot(xi,yi);
line([xi(1) xi(2)], [yi(1), yi(2)], [zi(1), zi(2)]);
line([xi(2) xi(3)], [yi(2), yi(3)], [zi(2), zi(3)]);
line([xi(3) xi(4)], [yi(3), yi(4)], [zi(3), zi(4)]);
line([xi(4) xi(1)], [yi(4), yi(1)], [zi(4), zi(1)]);
line([xi(5) xi(6)], [yi(5), yi(6)], [zi(5), zi(6)]);
line([xi(6) xi(7)], [yi(6), yi(7)], [zi(6), zi(7)]);
line([xi(7) xi(8)], [yi(7), yi(8)], [zi(7), zi(8)]);
line([xi(8) xi(5)], [yi(8), yi(5)], [zi(8), zi(5)]);
line([xi(1) xi(5)], [yi(1), yi(5)], [zi(1), zi(5)]);
line([xi(2) xi(6)], [yi(2), yi(6)], [zi(2), zi(6)]);
line([xi(3) xi(7)], [yi(3), yi(7)], [zi(3), zi(7)]);
line([xi(4) xi(8)], [yi(4), yi(8)], [zi(4), zi(8)]);
