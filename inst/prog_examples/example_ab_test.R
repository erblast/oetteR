

require(tidyverse)

# simulate drop in approved leads in bern

tib = tibble(  base_approval = 0.13
              , total_leads = 20:1000
              , percent_diff = list( seq(0.01, 0.13, 0.01) )
) %>%
  unnest(percent_diff, .drop = F) %>%
  mutate( regions = list( tibble( region = c('basel', 'bern' )
                           , lead_rate = c(231/326, 1-(231/326)) )
                        )
          ) %>%
  unnest( regions, .drop = F) 



tib_drop_bern = tib %>%
  mutate( n_approved = ifelse(region == 'basel'
                              , total_leads * lead_rate * base_approval
                              , total_leads * lead_rate * (base_approval - percent_diff) 
                              ) 
          , n_lost = total_leads * lead_rate - n_approved 
            ) %>%
  select( -lead_rate ) %>%
  group_by( base_approval, total_leads, percent_diff ) %>%
  nest()

chi_square = function(df){
  
  chi = df[, 2:3] %>%
    as.matrix() %>%
    chisq.test() %>%
    .$p.value

}

tib_drop_bern = tib_drop_bern %>%
  mutate( p_value = map(data, chi_square )
          , p_value = unlist(p_value) 
          )

ggplot( tib_drop_bern, aes( x = total_leads
                            , y = log( p_value )
                            , color = as.factor(percent_diff) 
                            ) 
        )+
  geom_line() +
  geom_hline( yintercept = c(log(0.05), log(0.005)) ) +
  geom_vline( xintercept = 500 )


# simulate drop in leads/pop in Bern

base = 0.0018

tib = tibble(  percent_basel = base
               , percent_bern = list( seq(0.0001, base, 0.0001) )
              ) %>%
  unnest(percent_bern, .drop = F) %>%
  mutate( regions = list( tibble( region = c('basel', 'bern' )
                                  , population = c(170000, 130000 )
                                )
                          )
          ) %>%
  unnest( regions, .drop = F) 

tib_drop_bern = tib %>%
  mutate( n_leads = ifelse(region == 'basel'
                              , population * percent_basel
                              , population * percent_bern
                            ) 
          , n_rest = population - n_leads
        ) %>%
  select( -population, - region ) %>%
  group_by( percent_basel, percent_bern ) %>%
  nest()

chi_square = function(df){
  
  chi = df %>%
    as.matrix() %>%
    chisq.test() %>%
    .$p.value
  
}

tib_drop_bern = tib_drop_bern %>%
  mutate( p_value = map(data, chi_square )
          , p_value = unlist(p_value) 
  )

p = ggplot( tib_drop_bern, aes( x = percent_bern
                            , y = log( p_value )
                          ) 
      )+
  geom_line()+
  geom_hline( yintercept = c(log(0.05), log(0.005) ) ) 

plotly::ggplotly(p)
