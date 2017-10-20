

context('test pca')


test_that( 'pca function'
           ,{

   data_ls = f_clean_data(mtcars)
      f_boxcox()

   # run the options
   f_pca(data_ls, use_boxcox_tansformed_vars = F)
   f_pca(data_ls, include_ordered_categoricals = F)

   pca_ls = f_pca(data_ls)
   pca    = pca_ls = pca

   expect_equal(pca$contrib$PC1, pca$rotation$PC1^2 /sum( pca$rotation$PC1^2 ) * 100)

   contrib_tot_pc1 = pca$contrib_abs_perc %>%
     filter(PC == 'PC1') %>%
     summarise( value = sum(value) )

   expect_equal(pca$vae$PC1, {  %>% filter(PC == 'PC1') sum(.$value) } )


})





