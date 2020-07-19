# function for getting values from broom df
x <- lm(Sepal.Width ~ Species, iris) %>% broom::tidy()
z <- lapply(as.list(x), function(z) {
  z <- as.list(z)
  names(z) <- x[["term"]]
  z
})
