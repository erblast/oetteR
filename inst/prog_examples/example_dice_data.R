

# assuming grouping variables are not ordered and do not exert a incremental effect
# on any response variable, it could be possible that the combinations of a grouping
# variable with any other variable could resort in a new group that exerts its own effect
# on any response variable


data = as_tibble( ISLR::Credit )

data_ls = f_clean_data(data, id_cols = 'ID') %>%
  f_boxcox

rename_leves = function(x){

  levels(x) = c('LL', 'LM', 'M', 'HM', 'HH' )

  return(x)

}

summary(data_ls$data$Gender)

# We do not find any difference in the dataset based on Gender
# through dicing we might pick up differences for example based on the
# combination of gender and ethnicity

df_stat = f_stat_anova(data_ls, col_group = 'Gender') %>%
  f_stat_combine_anova_with_chi_square( f_stat_chi_square(data_ls, 'Gender') ) %>%
  filter(p_value > 0.01 )

# transform all numeric variables to factors
# we use boxcox transformed values to have them
# more or less normally distributed

step1= data_ls$boxcox_data %>%
  as_tibble() %>%
  bind_cols( data_ls$data[,data_ls$ids] ) %>%
  mutate_if( is.numeric, cut, breaks = 5 ) %>%
  mutate_if( is.factor, rename_leves ) %>%
  gather( key ='key', value = 'value',- ID) %>%
  mutate( key = stringr::str_replace(key, 'boxcox', value))


# combine gender with income and check for new impact on credit rating
step2 = step1 %>%
  left_join(data_ls$data) %>%
  filter( startsWith(key, 'Income') ) %>%
  arrange(cylinders) %>%
  group_by( key, cylinders) %>%
  mutate_if( is.numeric,  function(x) ifelse( is.na(x), 0, x ) ) %>%
  summarize( mean_disp = mean(displacement)
             , n = n() ) %>%
  complete(key, cylinders)


