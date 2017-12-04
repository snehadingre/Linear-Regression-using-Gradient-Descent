library(ggplot2)
data<-read.csv("E:/Linear Regression using Gradient Descent/cryogenic flow data.csv")

#Feature Scaling
data$weight.scaled=(data$weight-mean(data$weight))/(max(data$weight)-min(data$weight))
data$pressure.scaled=(data$pressure-mean(data$pressure))/(max(data$pressure)-min(data$pressure))

#visualze Pressure vs Weight in Cryogenic Flow Meters
plot(data$weight.scaled~data$pressure.scaled,type="p")

#calculate gradient descent
m=length(data$weight.scaled)

X=matrix(c(1),nrow = m,ncol = 1)
P=matrix(data$pressure.scaled,nrow = m,ncol = 1) 
Y=matrix(data$weight.scaled,nrow = m,ncol = 1) 
Z=cbind(X,P)

#theta=c(rep(0,2))
theta=c(0,0)

iter=3000
alpha=0.01
i=1

#Computing cost
cost<-function(Z,Y,theta) {
  h_theta=Z %*% theta
  J=(1/(2*m))*sum((h_theta-Y)^2)
  return(J)
  
}

J.val<-cost(Z,Y,theta)
print(paste("Total cost computed for theta=[0;0] is:",J.val))

#apply gradient descent

J.history=c(rep(0,iter))
for(i in 1:iter) {
  theta=theta-alpha*(1/m*(t(Z)%*%Z%*%theta-t(Z)%*%Y))
  J.history[i]=cost(Z,Y,theta)
  print(J.history[i])
}

print(paste("Theta for which cost is minimum:"))
print(paste("theta 0:",theta[1],"and theta 1:",theta[2]))

plot(data$weight.scaled~data$pressure.scaled,type="p")
abline(a=theta[1],b=theta[2])

#calculate Residuals to evaluate the model
pred<-matrix(c(0),nrow = m,ncol = 1)
pred<-theta[1]+theta[2]*P

res<-Y-pred

ggplot(data, aes(x = pressure.scaled, y = weight.scaled)) +
  geom_segment(aes(xend = pressure.scaled, yend = pred))+
  geom_point() +
  geom_point(aes(y = pred), col="red",shape=1)
hist(res,density = 100,breaks =8,xlab = "Residuals",freq = FALSE)
curve(dnorm(x, mean=mean(res), sd=sqrt(var(res))),col="darkblue", lwd=2, add=TRUE, yaxt="n")
