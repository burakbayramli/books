"falling.rock" <-
function () 
{
x <- 1:5
y <- 0.5 * 9.8 * x^2
plot(y~x, xlab = "Time (sec)", ylab = "Distance fallen (m)", pch = 16)
lines(spline(x, y)) # Pass a smooth curve through the points
}
