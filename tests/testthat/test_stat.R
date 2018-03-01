

context('stat functions')

test_that('f_stat of different means'
  ,{

    set.seed(1)
    df = tibble( fct = sample(LETTERS[1:5], 100, replace = T)
                   , v1  = 1
                   , v2  = rnorm(100, 4)
                   , v3  = c( rep(3, 50), rep(8,50) )
                   )

    col_group = 'fct'

    f_stat_diff_of_means_medians(df, col_group, 'v1') %>%
    bind_rows( f_stat_diff_of_means_medians(df, col_group, 'v2') ) %>%
    bind_rows( f_stat_diff_of_means_medians(df, col_group, 'v3') )

})


test_that('f_stat_anova'
  ,{
    #regular dataset
    df_anova = data_ls = f_clean_data(mtcars) %>%
      f_stat_anova('cyl')

    df_anova

    #one constant, one continueous and one variable where all variance of one
    #variable can be explained by the difference in variance between groups


    size = 10000
    test_df = tibble( fct = as.factor( c( rep('a', size), rep('b', size), rep('c', size) ) )
                      , var1 = c( rep( 1, size), rep( 2, size), rep( 3, size) )
                      , var2 = c( rep( 1, 3 * size) )
                      , var3 = c( 1: (3 * size) )
                      , var4 = c( rep(1,size), rep(1,size), rep(1, size-3 ), rep(6, 3) )
                      , var5 = c( rep(1,size), rep(8,size), rep(1, size-3 ), rep(6, 3) )
    )


    f_stat_anova( f_clean_data(test_df, min_number_of_levels_nums = 1), 'fct' )

})


test_that('f_stat_max_diff_of_freq'
  ,{

  data_ls = f_clean_data(mtcars)
  df = f_stat_max_diff_of_freq(data_ls$data, 'cyl', 'gear')

})


test_that('f_stat_chi_square'
  ,{

  data_ls = f_clean_data(mtcars)
  df_chi_squ = f_stat_chi_square(data_ls, 'cyl')
})

test_that('f_stat_chi_square'
         ,{

   data_ls = f_clean_data(mtcars)
   df_chi_squ = f_stat_chi_square(data_ls, 'cyl')
   df_anova = f_stat_anova(data_ls, 'cyl')
   df_comb1 = f_stat_combine_anova_with_chi_square(df_anova, df_chi_squ)
   df_comb2 = f_stat_combine_anova_with_chi_square(df_anova)
   df_comb3 = f_stat_combine_anova_with_chi_square(df_chi_square = df_chi_squ)

   expect_equal( nrow(df_comb1), nrow(df_comb2) + nrow(df_comb3) )

})



test_that('f_stat_group'
          ,{
  data_ls = f_clean_data(mtcars)
  col_group = 'cyl'
  tresh_p_val = 0.05
  thresh_diff_perc = 3
  output_file = 'test_me'
  f_stat_group_ana(data_ls, col_group, tresh_p_val, thresh_diff_perc, output_file, quiet = TRUE)

  f_stat_group_ana(data_ls, col_group, tresh_p_val, thresh_diff_perc, output_file
                   , max_alluvial_flows = 20
                   , quiet = TRUE)

  file.remove('test_me.html')
  file.remove('test_me_stat_plots.html')
  file.remove('test_me_alluvial.html')
  file.remove('test_me_tabplots.html')

})


test_that( 'stat group means, medians, counts and percentages'
  ,{
    data_ls = f_clean_data(mtcars)
    f_stat_group_mean_medians(data_ls, 'cyl')
    f_stat_group_counts_percentages( data_ls, 'cyl')
})


test_that('shapiro'
  ,{

    f_stat_shapiro( rnorm(1000, 10, 1) )
    f_stat_shapiro( runif(1000, 1, 10) )
    m = f_stat_shapiro( rep(5, 100) )

    expect_true( is.na(m$statistic) )
    expect_true( is.na(m$p.value) )

})

