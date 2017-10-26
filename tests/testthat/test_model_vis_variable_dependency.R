

context('visualize model dependencies')

test_that('f_model_plot_variable_dependency_regression'
  ,{
    data_ls = f_clean_data(mtcars)
    m = rpart::rpart(disp~., data_ls$data)
    imp = f_model_importance(m, data_ls$data)
    col_vector = f_plot_color_code_variables(data_ls)
    p = f_model_plot_variable_dependency_regression(data = data_ls$data
                                                    , model = m
                                                    , ranked_variables = imp
                                                    , numericals = data_ls$numericals
                                                    , colvector = col_vector
                                                    )

  })
