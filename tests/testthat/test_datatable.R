
context('datatable wrappers')

test_that('f_datatable_universal'
  ,{

  data_ls = f_clean_data(mtcars)
  f_stat_group_counts_percentages(data_ls, 'cyl') %>%
    f_datatable_universal()

  f_stat_group_mean_medians(data_ls, 'cyl') %>%
    f_datatable_universal(round_other_nums = 2)

  f_datatable_universal(mtcars, round_other_nums = 1)

  # test if function works with other variable types

  data = as_tibble(mtcars)
  data$names_chr = row.names(mtcars)
  data$names_fct = as.factor( row.names(mtcars) )

  f_datatable_universal(data, round_other_nums = 1)

  # test if function works with NA vales

  data$cyl[1:4] = NA

  f_datatable_universal(data, round_other_nums = 1)

})
