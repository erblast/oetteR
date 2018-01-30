

ks.test( rgamma(50000, 0.5), ggplot2::diamonds$price )
ks.test( rgamma(50000, 0.75), ggplot2::diamonds$price )
ks.test( rgamma(50000, 1), ggplot2::diamonds$price )
ks.test( rgamma(50000, 1.25), ggplot2::diamonds$price )
ks.test( rgamma(50000, 1.5), ggplot2::diamonds$price )
ks.test( rgamma(50000, 2), ggplot2::diamonds$price )
ks.test( rgamma(50000, 2.5), ggplot2::diamonds$price )




ks.test( rgamma(50000, 1), MASS::quine$Days )
ks.test( rgamma(50000, 1), MASS::quine$Days )
ks.test( rgamma(50000, 1), rgamma(50000, 1) )

hist(rgamma(50000, 1))
hist(ggplot2::diamonds$price)


t = tibble( rate = seq(0.1,5, 0.05)
            , shape = list(seq(0.1,5, 0.1) )
)%>%
  unnest(shape)%>%
  mutate( test = map2( rate, shape, function(x,y)  ks.test( ggplot2::diamonds$price, 'pgamma', x, y ) )
          ,p_val = map_dbl(test, 'p.value')
          ,D = map_dbl(test,'statistic')
          )

t = t %>%
  select(-test)

summary(t)

t %>%
  filter(p_val <= 0.0001 ) %>%
  filter(D == min(D) )


t = tibble( rate = seq(0.1,5, 0.05)
            , shape = list(seq(0.1,5, 0.1) )
)%>%
  unnest(shape)%>%
  mutate( test = map2( rate, shape, function(x,y)  ks.test( MASS::quine$Days, 'pgamma', x, y ) )
          ,p_val = map_dbl(test, 'p.value')
          ,D = map_dbl(test,'statistic')
  ) %>%
  select(-test)

summary(t)

t %>%
  filter(p_val <= 0.0001 ) %>%
  filter(D == min(D) )

