



add_mean = function(df, var){

  var = enquo(var)

  df = df %>%
    mutate( mean = map( !! var, mean) )


  return(df)

}

df = mtcars

df %>%
  add_mean(disp)


add_mean = function(df, var1, var2, var3){

  var1 = enquo(var1)
  var2 = enquo(var2)
  var3 = enquo(var3)

  df = df %>%
    mutate( add = pmap( list(!!var1, !!var2, !!var3), function(x,y,z) x+y+z ) )


  return(df)

}

df = mtcars

df %>%
  add_mean(disp, mpg, hp)
