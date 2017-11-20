

query = '
SELECT	DK.*, KI.*, S.device
FROM jemas_temp.ob.BASE_DYNAMIC_konto_info		DK
LEFT JOIN jemas_temp.ob.BASE_konto_info			KI	ON KI.konto_lauf_id = DK.konto_lauf_id_DYN
LEFT JOIN jemas_temp.mz.PERSON_SMARTPHONE_171109 S	ON S.person_id		= KI.person_id_hk_inhaber
WHERE device IS NOT NULL
ORDER BY KI.erste_karte_versand_datum DESC, anz_dd
'

con = bcagR::jemas_connect()

df = jemas_select(con, query)

bcagR::jemas_close(con)

df = select(df, -produkt_id)

data_ls = oetteR::f_clean_data( df ,exclude_missing = )


f_anova_stats( sample_n(data_ls$data, size = 20000 ), 'device', data_ls$numericals[54])

summary( data_ls$data )

sd( data_ls$data[[data_ls$numericals[54]]] )


data_ls$data %>%
  select( device, eingegangene_zahlungen) %>%
  group_by( device )%>%
  summarise( sd = sd(eingegangene_zahlungen)
             , mean = mean(eingegangene_zahlungen) )



test_df = tibble( fct = as.factor(c( 'a','a','a','b','b','b','c','c','c'))
                  , var1 = c(1,1,1,2,2,2,3,3,3)
                  , var2 = c(1,1,1,1,1,1,1,1,1)
                  , var3 = c(1,2,3,4,5,6,7,8,9)
                  , var4 = c(1,1,1,1,1,1,3,4,5) 
                  ) 

f_anova_stats( test_df, group = 'fct', names(test_df)[ ! names(test_df) %in%  'fct' ] )

size = 100000
test_df = tibble( fct = as.factor( c( rep('a', size), rep('b', size), rep('c', size) ) )
                  , var1 = c( rep( 1, size), rep( 2, size), rep( 3, size) )
                  , var2 = c( rep( 1, 3 * size) )
                  , var3 = c( 1: (3 * size) )
                  , var4 = c( rep(1,size), rep(1,size), rep(1, size-3 ), rep(6, 3) ) 
                  , var5 = c( rep(1,size), rep(8,size), rep(1, size-3 ), rep(6, 3) ) 
) 


f_anova_stats( test_df, group = 'fct', names(test_df)[ ! names(test_df) %in%  'fct' ] )

test_df %>%
  group_by(fct) %>%
  summarise_all( mean )

test_df %>%
  group_by(fct) %>%
  summarise_all( sd )







