switchcount <- function(s,g){


n <- length(s);
m <- length(g);
swt <- matrix(data=0,nrow=m,ncol=m) 
t <- 2;

while (t<=n){

st1 <- s[t-1]
st<- s[t]
swt[st1,st] <- swt[st1,st] + 1
t <- t+ 1;
}

return(swt)

}
