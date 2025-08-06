Install.package("geepack)
library(geepack)
#Example 2
data(dietox)
head(dietox)
## Not run:
if (require(ggplot2)){
  qplot(Time, Weight, data=dietox, col=Pig) + geom_line() +
    theme(legend.position= "none") + facet_grid(Evit~Cu)
} else {
  coplot(Weight ~ Time | Evit* Cu, data=dietox)
}
## End(Not run)

data(dietox)

dietox$Cu<-as.factor(dietox$Cu)

mf <-formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))

gee1 <-geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
gee1
summary(gee1)
coef(gee1)
vcov(gee1)
summary(gee1)
coef(summary(gee1)) #different p-value

mf2 <-formula(Weight ~ Cu * Time + I(Time^2) + I(Time^3))
gee2 <-geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
summary(gee2)
anova(gee2)

#Clustered Data
data(respdis)
resp.l<-reshape(respdis, varying =list(c("y1", "y2", "y3", "y4")),
                v.names= "resp", direction = "long")
resp.l<-resp.l[order(resp.l$id, resp.l$time),]
fit <-ordgee(ordered(resp) ~ trt, id=id, data=resp.l, int.const=FALSE)
summary(fit)
data(ohio)
ohio$resp<-ordered(as.factor(ohio$resp))
fit <-ordgee(resp~ age + smoke + age:smoke, id = id, data=ohio)
summary(fit)