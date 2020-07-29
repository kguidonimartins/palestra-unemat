
pico_plot <- function(zlab, color) {

  # http://www.ejwagenmakers.com/misc/Plotting_3d_in_R.pdf
  mu1 <- 0 # setting the expected value of x1
  mu2 <- 0 # setting the expected value of x2
  s11 <- 10 # setting the variance of x1
  s12 <- 15 # setting the covariance between x1 and x2
  s22 <- 10 # setting the variance of x2
  rho <- 0.5 # setting the correlation coefficient between x1 and x2
  x1 <- seq(-10, 10, length = 41) # generating the vector series x1
  x2 <- x1 # copying x1 to x2

  # setting up the function of the multivariate normal density
  f <- function(x1, x2) {
    term1 <- 1 / (2 * pi * sqrt(s11 * s22 * (1 - rho^2)))
    term2 <- -1 / (2 * (1 - rho^2))
    term3 <- (x1 - mu1)^2 / s11
    term4 <- (x2 - mu2)^2 / s22
    term5 <- -2 * rho * ((x1 - mu1) * (x2 - mu2)) / (sqrt(s11) * sqrt(s22))
    term1 * exp(term2 * (term3 + term4 - term5))
  }

  z <- outer(x1, x2, f) # calculating the density values

  if (color == "solo") {
    jet.colors <- colorRampPalette(c("#00A600", "#3EBB00", "#8BD000", "#E6E600", "#E9BD3A", "#ECB176", "#EFC2B3", "#F2F2F2"))
  }

  if (color == "clima") {
    jet.colors <- colorRampPalette(c("#4B0055", "#3C3777", "#006290", "#008A98", "#00AC8E", "#25C771", "#A6DA42", "#FDE333"))
  }

  if (color == "abundancia") {
    jet.colors <- colorRampPalette("grey")
  }

  nrz <- nrow(z)
  ncz <- ncol(z)
  nbcol <- 64
  color <- jet.colors(nbcol)
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  facetcol <- cut(zfacet, nbcol)

  persp(x1, x2, z,
    main = "",
    xlab = "Latitude",
    ylab = "Longitude",
    zlab = zlab,
    col = color[facetcol],
    theta = 30, phi = 30,
    r = 50,
    d = 0.1,
    expand = 0.5,
    ltheta = 90, lphi = 180,
    shade = 0.75,
    ticktype = "simple",
    nticks = 5
  )
}


ppi <- 300
png("images/3D-abundancia.png", width = (20 / 2.54) * ppi, height = (20 / 2.54) * ppi, res = ppi)
pico_plot(zlab = "Abundância", color = "abundancia")
dev.off()

