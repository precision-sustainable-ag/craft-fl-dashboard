clampedColorNumeric = 
  function(
    palette, domain, na.color = "#808080", alpha = FALSE, 
    reverse = FALSE
  ) {
    rng <- NULL
    if (length(domain) > 0) {
      rng <- range(domain, na.rm = TRUE)
      if (!all(is.finite(rng))) {
        stop("Wasn't able to determine range of domain")
      }
    }
    pf <- leaflet:::safePaletteFunc(palette, na.color, alpha)
    leaflet:::withColorAttr("numeric", list(na.color = na.color), function(x) {
      if (length(x) == 0 || all(is.na(x))) {
        return(pf(x))
      }
      if (is.null(rng)) 
        rng <- range(x, na.rm = TRUE)
      rescaled <- scales::rescale(x, from = rng) %>% 
        scales::oob_squish()
      if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE)) 
        warning("Some values were outside the color scale and will be treated as NA")
      if (reverse) {
        rescaled <- 1 - rescaled
      }
      pf(rescaled)
    })
  }


rescale_bins = function(x, brks, to = 0:1) {
  brks = brks[is.finite(brks)]
  x = c(min(brks), max(brks), x)
  x = scales::oob_squish(x, range = range(brks), only.finite = F)
  brks = unique(c(-Inf, brks, Inf))
  idx = findInterval(x, brks)
  bin_scaled = (x - brks[idx]) / (brks[idx+1] - brks[idx]) + idx - 1
  tail(scales::rescale(bin_scaled, to), -2)
}  


binnedColorNumeric = 
  function(
    palette, na.color = "#808080", alpha = FALSE, 
    reverse = FALSE, bins
  ) {
    rng <- NULL
    pf <- leaflet:::safePaletteFunc(palette, na.color, alpha)
    leaflet:::withColorAttr("numeric", list(na.color = na.color), function(x) {
      if (length(x) == 0 || all(is.na(x))) {
        return(pf(x))
      }
      if (is.null(rng)) 
        rng <- range(x, na.rm = TRUE)
      rescaled <- rescale_bins(x, bins)
      if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE)) 
        warning("Some values were outside the color scale and will be treated as NA")
      if (reverse) {
        rescaled <- 1 - rescaled
      }
      pf(rescaled)
    })
  }

plot_breaks = function(brks) {
  brks = brks[is.finite(brks)]
  ys = seq(0, 1, length.out = length(brks))
  list(
    ggplot2::geom_vline(xintercept = brks, alpha = 0.5),
    ggplot2::annotate(
      "segment",
      x = head(brks, -1),
      xend = tail(brks, -1),
      y = head(ys, -1),
      yend = tail(ys, -1),
      alpha = 0.7, linetype = "dashed"
    )
  )
}