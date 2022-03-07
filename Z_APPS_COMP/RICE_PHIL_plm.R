# load the data set
library( "frontier" )
data( "riceProdPhil" )

# Estimate a Cobb-Douglas production function with fixed individual effects
library( "plm" )
riceCdFix <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
   data = riceProdPhil, index =  c( "FMERCODE", "YEARDUM" ) )
summary( riceCdFix )
# alternatively
pDat <- plm.data( riceProdPhil, c( "FMERCODE", "YEARDUM" ) )
riceCdFixPd <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
   data = pDat )
summary( riceCdFixPd )

# Estimate a Cobb-Douglas production function with fixed two-ways effects
riceCdFix2 <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
    data = riceProdPhil, index =  c( "FMERCODE", "YEARDUM" ),
    effect = "twoways" )
summary( riceCdFix2 )
# alternatively
riceCdFixPd2 <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
    data = pDat, effect = "twoways" )
summary( riceCdFixPd2 )
 
# Extract individual fixed effects
fixef( riceCdFix2 )
# Extract time fixed effects
fixef( riceCdFix2, effect = "time" )

# Statistical significance of the fixed individual and time effects
# Estimate pooled model
riceCdPool <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
    data = riceProdPhil, model = "pooling" )
summary( riceCdPool )
# alternatively
riceCdPoolPd <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
    data = pDat, model = "pooling" )
summary( riceCdPoolPd )
# Time effects
pooltest( riceCdFix, riceCdFix2 )
# Individual & time effects
pooltest( riceCdPool, riceCdFix2 )
# Individual effects
pooltest( riceCdPool, riceCdFix )

# Estimate a Cobb-Douglas production function with random individual effects
riceCdRan <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
   data = riceProdPhil, index =  c( "FMERCODE", "YEARDUM" ),
   model = "random" )
summary( riceCdRan )
# alternatively
riceCdRanPd <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
   data = pDat, model = "random" )
summary( riceCdRanPd )

# Hausman test for the consistency of the random-effects estimators
phtest( riceCdFix, riceCdRan )

# Estimate a Cobb-Douglas production function with random two-ways effects
riceCdRan2 <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
    data = riceProdPhil, index =  c( "FMERCODE", "YEARDUM" ),
    effect = "twoways", model = "random" )
summary( riceCdRan2 )
# alternatively
riceCdRanPd2 <- plm( log( PROD ) ~ log( AREA ) + log( LABOR ) + log( NPK ), 
    data = pDat, effect = "twoways", model = "random" )
summary( riceCdRanPd2 )
# 
# Hausman test for the consistency of the random-effects estimators
phtest( riceCdFix2, riceCdRan2 )
