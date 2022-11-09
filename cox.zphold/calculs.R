library(survival)


source("D:/D/Marc/SMS/FORMATIONS/analyse_duree/cox.zphold/cox.zphold.R")


test1 <- data.frame(time=  c(1,2,3,4,5,6,7),
                    status=c(1,0,0,1,1,1,0),
                    x=     c(1,0,0,1,1,0,1))
fit = coxph(formula = Surv(time, status) ~ x, data=test1)
fit
cox.zph(fit, transform="identity")
cox.zphold(fit, transform="identity")


resid = resid(fit, type="schoenfeld")
resid
residsc = resid(fit, type="scaledsch")
residsc

# t= 1
a1 = 4*(exp(0.6217)) + 3
b1 = 4*exp(0.6217)
c1 = b1/a1
r1 = 1 - c1
i1 = c1 - c1^2
i1
v1 = 1/i1

rs1= fit$coefficients + 4*fit$var*r1

# t = 4
a2 = 3*(exp(0.6217)) + 1
b2 = 3*exp(0.6217)
c2 = b2/a2
r2 = 1 - c2
i2 = c2 - c2^2
i2
v2 = 1/i2

rs2=fit$coefficients + 4*fit$var*r2

# t = 5
a3 = 2*(exp(0.6217)) + 1
b3 = 2*exp(0.6217)
c3 = b3/a3
r3 = 1 - c3
i3 = c3 - c3^2
i3
v3 = 1/i3

rs3=fit$coefficients + 4*fit$var*r3

# t = 5
a4 = (exp(0.6217)) + 1
b4 = exp(0.6217)
c4 = b4/a4
r4 = -c4
i4 = c4 - c4^2
i4
v4 = 1/i4

rs4=fit$coefficients + 4*fit$var*rs4
rs4

g = c(1,4,5,6) # death times
U = c(r1,r2,r3,r4)
imat = c(i1,i2,i3,i4)
V=1/imat
#imat = c(fit$var,fit$var,fit$var,fit$var)

u2 <- c(0, sum(g*U))  # first derivative
I2 <- matrix(c(sum(1/V), sum(g*(1/V)), sum(g*(1/V)), sum(g^2*(1/V))),2,2)  # second derivative
I2


M =solve(I2, u2)
M %*% u2
sctest <- solve(I2, u2) %*% u2
sctest

(sum(g*U))^2

A = sum(g*V*g)

a= sum(g*V)
a
b = 1/sum(V)
c = 





# Test standard
#ttimes = c(-3,0,1,2)
ttimes = c(-3,0,1,2)
num = sum(residsc*ttimes)^2
num
ttimes2 = ttimes^2
denom = (4*fit$var)*sum(ttimes2)
num/denom

# test exact

invV = 1/V

ttimes = c(-3,0,1,2)


A = sum(ttimes*V*ttimes)
A
B = sum(ttimes*V)
C = 1/(sum(V))
D = sum(t(ttimes*V))
D
B*C*D
denom = A - B*C*D
denom



X = sum(U*ttimes)
X
X*(1/denom)*t(X)

sum((U*ttimes)^2)
num = (sum(residsc*ttimes))^2
num

num/denom

t(sum(U*ttimes))*(1/denom)*(sum(U*ttimes))

t(sum(ttimes*U))*(D^(-1))*(sum(ttimes*U))

D=sum(ttimes*V*ttimes) - (sum(ttimes*V))*(1/sum(V))*t(sum(ttimes*V))
D

sum(ttimes*V*ttimes) 
(sum(ttimes*V))*(1/sum(V))*t(sum(ttimes*V))
