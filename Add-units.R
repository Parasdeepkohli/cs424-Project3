addUnits <- function(n) {
  labels <- ifelse((n<1e6) & (n > 0), paste0(round(n/1e3), ' thousand'), # in thousands
                          ifelse((n < 1e7) & (n > 0), paste0(round(n/1e3)/1000, ' Million'), # To handle decimal millions
                            ifelse((n < 1e9) & (n > 0), paste0(round(n/1e6), ' Million'),  # in millions
                                 ifelse((n < 1e12) & (n > 0), paste0(round(n/1e6)/1000, ' Billion'), '0' # in Billions
                                        ))))
  return(labels)
}