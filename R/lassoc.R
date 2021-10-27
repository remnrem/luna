outlier <- function(x,t=3) { mean(x,na.rm=T) + t * sd(x,na.rm=T) }

outliers <- function(x,m =mean(x,na.rm=T) , sdev = sd(x,na.rm=T) ,t=3)
{
 lwr <- m - t * sdev
 upr <- m + t * sdev
 x[ x < lwr ] <- NA
 x[ x > upr ] <- NA
 x
}

outliers.inc <- function(x, inc = rep(T,length(x)) , m =mean(x[inc],na.rm=T) , sdev = sd(x[inc],na.rm=T) ,t=3)
{
 lwr <- m - t * sdev
 upr <- m + t * sdev
 x[ x < lwr ] <- NA
 x[ x > upr ] <- NA
 x[ ! inc ]   <- NA
 x
}

is.outlier <- function(x,m =mean(x,na.rm=T) , sdev = sd(x,na.rm=T) ,t=3)
{
is.na( (!is.na(x)) & outliers(x,m,sdev,t))
}

is.outlier.inc <- function(x,inc=inc,t=3)
{
m = mean(x[inc],na.rm=T)
sdev = sd(x[inc],na.rm=T)
(!inc) | is.na( (!is.na(x)) & outliers(x,m,sdev,t))
}

## Normalisation

zf <- function( x , m=mean(x,na.rm=T), sdev=sd(x,na.rm=T) ) { ( x- m ) / sdev }


# univariate , t-test on disease  (and plot)
# assumes grp coded 0 / 1 (where '1' is 'disease', 0 is control)
# nb. return -1 * t so that reference is CASE vs CON, not other way around

f1 <- function( df , dv , grp , plotme=F) {
tt <- t.test(  outliers( df[ , dv ] ) ~ df[,grp ]  )
if ( plotme ) boxplot( outliers( df[ , v ] ) ~ df[,grp] + 1 , col = c( "blue", "gray" ) , main=v , xlab = c("p=",signif(tt$p.value,3)) , ylab=v )
cat( v , tt$p.value , "\n" )
c(mean( outliers( df[ , dv ] ),na.rm=T), tt$estimate , -1*tt$statistic , tt$p.value)
}

f1.all <- function( df , vars , grp ) {
res <- numeric()
for (v in vars ) res <- rbind( res , f1(df,v,grp) )
df2 <- data.frame( vars , res )
names(df2) <- c("VAR" , "M" , "HC","SCZ", "T" , "P" )
df2$VAR <- as.character( df2$VAR )
df2
}

# test for disease stats, control AGE and SEX
f1c <- function( df , dv , grp ) {
y <- outliers( m[,dv] )
if ( var(y,na.rm=T) < 1e-8 ) return(c(NA,NA))
pv <- summary(lm( zf(y) ~ df[,grp] + AGE + SEX , data = df ) )$coef[2,c(1,4)]
cat( dv , pv[1] , pv[2] , "\n" )
pv
}

f1c.all <- function( df, vars ,grp ) {
res <- numeric()
for (v in vars ) res <- rbind( res , f1c(df,v,grp) )
df2 <- data.frame( vars , res )
names(df2) <- c("VAR" , "B" , "P" )
df2
}


### not changed...

# generic function: y ~ X
f2 <- function(dv,iv,plotme=F,th=3) {
pv <- summary(lm( outliers( m[,dv] , t = th )  ~ outliers( m[ , iv ] , t = th )  , data = m ) )$coef[2,4]
if (plotme)
 plot( m[,iv],m[,dv],
       xlab=iv, ylab=dv,
       col = 1 + ( is.outlier( m[,dv] , t = th ) | is.outlier( m[,iv], t = th ) ) , pch=20 )
cat( dv , iv , pv , "\n" )
}


# case only
f2co <- function(dv,iv) {
pv <- summary(lm( outliers( m[,dv] )  ~ outliers( m[ , iv ] )  , data = m , subset = DIS == 1 ) )$coef[2,4]
cat( dv , iv , pv , "\n" )
}


# control for disease stats, age and sdex
f2con <- function(dv,iv) {
pv <- summary(lm( outliers( m[,dv] )  ~ outliers( m[ , iv ] ) + DIS + age + gender , data = m ) )$coef[2,4]
cat( dv , iv , pv , "\n" )
}

# simple correlation
fr <- function( dv , iv ) {
rr <- cor.test( outliers( m[,dv] ) , outliers( m[,iv] ) )
cat( dv ,iv , rr$estimate , rr$p.value , "\n" )
c( rr$estimate , rr$p.value )
}

# case only correlation
fr.co <- function( dv , iv ) {
rr <- cor.test( outliers( m[m$DIS==1,dv] ) , outliers( m[m$DIS==1,iv] ) )
c( rr$estimate , rr$p.value )
}


fr.all <- function( var1 , vars ) {
res <- numeric()
for (v in vars ) {
cat(v,"\n")
res <- rbind( res , fr( var1 , v) )
}
df <- data.frame( vars , res )
names(df) <- c("VAR" , "R" , "P" )
df$Z <- sign( df$R ) * -log10( df$P ) 
df
}


