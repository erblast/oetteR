

# pl = pipelearner::pipelearner(mtcars) %>%
#   pipelearner::learn_models( twidlr::rpart, form ) %>%
#   pipelearner::learn_models( twidlr::randomForest, form ) %>%
#   pipelearner::learn_models( twidlr::svm, form ) %>%
#   pipelearner::learn() %>%
#   mutate( preds = map2(fit, test, predict) ) %>%
#   unnest( preds . )
#
#
# f_predict_regression = function( fit, data, target, id_cols){
#
#   df = data %>%
#     as.data.frame() %>%
#     mutate( pred = predict(fit, .) ) %>%
#     select( one_of( c(id_cols, target, ),  )
#
# }
