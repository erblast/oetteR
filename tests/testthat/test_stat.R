

context('stat functions')

testthat('f_stat of different means'
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


testthat('f_stat_anova'
  ,{
    #regular dataset
    df_anova = data_ls = f_clean_data(mtcars) %>%
      f_stat_anova('cyl')

    df_anova

    #one constant, one continueous and one variable where all variance of one
    #variable can be explained by the difference in variance between groups

    set.seed(1)
    df = tibble( fct = c( rep(LETTERS[1], 5), rep(LETTERS[2],5) )
                 , v1  = 1
                 , v2  = rnorm(10, 4)
                 , v3  = c( rep(3, 5), rep(8,5) )
    )

    col_group = 'fct'

    df_anova = f_clean_data(df, min_number_of_levels_nums = 1) %>%
      f_stat_anova('fct')

  })


testthat('f_stat_max_diff_of_freq'
  ,{

  data_ls = f_clean_data(mtcars)
  df = f_stat_max_diff_of_freq(data_ls$data, 'cyl', 'gear')
  df

  })


testthat('f_stat_chi_square'
  ,{

  data_ls = f_clean_data(mtcars)
  df_chi_squ = f_stat_chi_square(data_ls, 'cyl')
  df_chi_squ
})

testthat('f_stat_group_ana_taglist'
  ,{
    data_ls = f_clean_data(mtcars)
    taglist = f_stat_group_ana_taglist(data_ls, 'cyl')
    f_plot_obj_2_html(taglist, type = "taglist", output_file = 'test_me', title = 'Plots')
    file.remove('test_me.html')
  })



