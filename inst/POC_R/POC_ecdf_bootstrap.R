
require(tidyverse)

df = tibble( values = rnorm(n = 500) )

df



get_stats = function( x, ix, intervalls = c(0.025, 0.125, 0.5, 0.875, 0.975) ){

  values = x[ix]

  df = tibble( values = values )

  pi = quantile(df$values, probs = intervalls)

  me = mean(df$values)

  std = sd(df$values)

  get_percentile = ecdf(df$values)

  return( c(me, std, pi, map_dbl(x, get_percentile)) )

}

b = boot::boot(data = rnorm(n = 500), statistic = get_stats
           , R = 1000 #, parallel = 'snow', ncpus = 4
           )

as_tibble(b)

broom::tidy(b)



