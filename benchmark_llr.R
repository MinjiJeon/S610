# Add code to the benchmark_file that computes how long your llr function takes to run and prints it out.

# hint: use the function cat and one of bench::mark or microbenchmark::microbenchmark)


setwd("/Users/camille/S610")
import::here(compute_f_hat, llr, make_predictor_matrix, make_weight_matrix, W,
             .from = 'llr_functions.R')

# --- example 1 --- #

# get the data
data(french_fries, package = 'reshape2')
french_fries <- na.omit(french_fries)

# input data
x1 <- french_fries$potato
y1 <- french_fries$buttery

# space along which to smooth
z1 <- seq(0, 15, length.out = 100)

# run smoothing
fits.1 <- llr(z = z1, x = x1, y = y1, omega = 2)

# plot the data and the smoother
plot(x1, y1)
lines(z, fits.1, col = 'red')


# --- example 2 --- #

# noisy sine wave
x2 <- runif(1000, -2 * pi, 2 * pi)
y2 <- sin(x2) + rnorm(length(x2))

# space along which to smooth
z2 <- seq(-2 * pi, 2 * pi, length.out = 100)

# run smoothing
fits.2 <- llr(z = z2, x = x2, y = y2, omega = pi / 3)

# plot the data and the smoother
plot(x2, y2)
lines(z2, fits.2, col = 'red')

library(microbenchmark)
microbenchmark(
  fits.1 <- llr(z = z1, x = x1, y = y1, omega = 2),
)

library(profvis)
profvis(llr(z = z1, x = x1, y = y1, omega = 2))


cat()
