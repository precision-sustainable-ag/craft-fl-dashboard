clampedColorNumeric = function (
    palette, domain, na.color = "#808080", alpha = FALSE, reverse = FALSE
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