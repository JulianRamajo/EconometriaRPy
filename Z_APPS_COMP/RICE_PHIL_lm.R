# load the data set
library( "frontier" )
data( "riceProdPhil" )

#apply R function "names()" to dataframe to see names of the variables 
names( riceProdPhil)

# estimate a Cobb-Douglas production function with two inputs
riceCd2 <- lm( log( PROD ) ~ log( AREA ) + log( LABOR ), 
   data = riceProdPhil )
summary( riceCd2 )

# estimate a Cobb-Douglas production function with four inputs
riceCd4 <- lm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ) + log( OTHER), 
   data = riceProdPhil )
summary( riceCd4 )

# Wald tests
anova( riceCd2, riceCd4 )

library( "car" )
linearHypothesis( riceCd4, c( "log(NPK) = 0", "log(OTHER) = 0" ) ) 

library( "lmtest" )
waldtest( riceCd2, riceCd4 )
waldtest( riceCd2, riceCd4, test = "Chisq" )

# LR test
lrtest( riceCd2, riceCd4 )

# Ramsey's RESET
resettest( riceCd2 )
resettest( riceCd4 )
 # Variant of Ramsey's RESET
resettest( riceCd2, type = "regressor" )
resettest( riceCd4, type = "regressor" )

# Durbin-Watson test
dwtest( riceCd2 )
dwtest( riceCd4 )

# Breusch-Pagan Test
bptest( riceCd2 )
bptest( riceCd4 )

# Tests with heteroskadasticity-consistent covariance matrix
library( "sandwich" )
coeftest( riceCd4, vcov = vcovHC )
waldtest( riceCd2, riceCd4, vcov = vcovHC )
