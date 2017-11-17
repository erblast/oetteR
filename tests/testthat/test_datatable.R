
context('datatable wrappers')

test_that('f_datatable_universal'
  ,{

  data_ls = f_clean_data(mtcars)
  f_stat_group_counts_percentages(data_ls, 'cyl') %>%
    f_datatable_universal()

  f_stat_group_mean_medians(data_ls, 'cyl') %>%
    f_datatable_universal(round_other_nums = 2)

})
