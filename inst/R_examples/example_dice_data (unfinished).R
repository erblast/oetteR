

# assuming grouping variables are not ordered and do not exert a incremental effect
# on any response variable, it could be possible that the combinations of a grouping
# variable with any other variable could resort in a new group that exerts its own effect
# on any response variable


data = as_tibble( ISLR::Credit )

data_ls = f_clean_data(data, id_cols = 'ID') %>%
  f_boxcox

rename_leves = function(x){

  levels(x) = c('L','M','H' )

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
  mutate_if( is.numeric, cut, breaks = 3 ) %>%
  mutate_if( is.factor, rename_leves ) %>%
  bind_cols( data_ls$data[,data_ls$ids] ) %>%
  gather( key ='key', value = 'value',- ID) %>%
  mutate( key = stringr::str_replace(key, 'boxcox', value))


# combine gender with income and check for new impact on credit rating

m = lm(Rating~Income, data_ls$data)

step2 = step1 %>%
  left_join(data_ls$data) %>%
  filter( startsWith(key, 'Income') ) %>%
  complete(key, Gender) %>%
  mutate( new_factor     = paste0(key, '_', Gender) )%>%
  modelr::add_residuals( m, 'rating_resid') %>%
  select( new_factor, rating_resid, Rating, Gender, value, Income )


data_ls_step2 = f_clean_data(step2)

f_stat_anova( data_ls_step2, col_group = 'new_factor')

f_stat_group_mean_medians( data_ls_step2, 'new_factor' )

# We find that after deducting the confounding effects of income from
# rating the combination of income and gender still does not have an effect

# Similarily we do not find an effect of Gender on Student, however we might
# find that potentially we might find something if we throw the income variable
# into the mix


step3 = step1 %>%
  left_join(data_ls$data) %>%
  filter( startsWith(key, 'Income') )

chi = chisq.test( table(select(step3, key, Student) ), p = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,'')  )


  %>%
  complete(key, Gender)

  %>%
  mutate( new_factor     = paste0(key, '_', Gender) )%>%
  modelr::add_residuals( m, 'rating_resid') %>%
  select( new_factor, rating_resid, Rating, Gender, value, Income )


