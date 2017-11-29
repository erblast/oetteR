

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

p <- qplot(1,1)

g = arrangeGrob( p,p,p,p , nrow = 2, ncol = 2 )

grid.arrange( g )


lay = rbind( c(1,1)
             ,c(2,3)
             )

g = arrangeGrob( p,p,p , layout_matrix = lay )

grid.arrange( g , main = 'Main Title')
