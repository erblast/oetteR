
data = ISLR::Auto %>%
  mutate( name = as.character(name) )

data_ls = f_clean_data(data, id_cols = 'name') %>%
  f_boxcox

rename_leves = function(x){

  levels(x) = c('LL', 'LM', 'M', 'HM', 'HH' )

  return(x)

}

step1= data_ls$boxcox_data %>%
  as_tibble() %>%
  bind_cols( data_ls$data[,data_ls$ids] ) %>%
  mutate_if( is.numeric, cut, breaks = 5 ) %>%
  mutate_if( is.factor, rename_leves ) %>%
  gather( key ='key', value = 'value',- name) %>%
  mutate( key = stringr::str_replace(key, 'boxcox', value))


step2 = step1 %>%
  left_join(data_ls$data) %>%
  filter( startsWith(key, 'mpg') ) %>%
  arrange(cylinders) %>%
  group_by( key, cylinders) %>%
  mutate_if( is.numeric,  function(x) ifelse( is.na(x), 0, x ) ) %>%
  summarize( mean_disp = mean(displacement)
             , n = n() ) %>%
  complete(key, cylinders)
