?cars
View(cars)
str(cars)
dim(cars)

plot(dist~speed,data=cars,xlab="stopping",main="stoping dist vs speed",pch=20,cex=2,col="green")

#speed = predictor variable,stopping distance
x=cars$speed
y=cars$dist
#least square method
sxy=sum((x-mean(x))*(y-mean(y)))
sxx=sum((x-mean(x))^2)
syy=sum((y-mean(y))^2)
c(sxy,sxx,syy)


#calculate beta0 and beta1
beta_i_hat=sxy/sxx
beta_0_hat=mean(y)-beta_i_hat*mean(x)
c(beta_0_hat,beta_i_hat)

#possible values in cara data
unique(cars$speed)
range(cars$dist)

#making a prediction of distance for the speed 8miles per hour
#y=-17.58+3.93*8
beta_0_hat+beta_i_hat*8

8%in% unique(cars$speed)

21%in% unique(cars$speed)

15%in% unique(cars$speed)

min(cars$speed)<21 & 21<max(cars$speed)

beta_0_hat+beta_i_hat*21

