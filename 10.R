# (7)
set.seed(0)
# Scale each observation (not the features):
USA_scaled = t(scale(t(USArrests)))
USA_scaled_ = scale(USArrests)

# The correlation of each sample with the other samples:
# 
Rij = cor(t(USA_scaled)) # -1 <= Rij <= +1 
OneMinusRij = 1 - Rij # 0 <= 1-Rij <= +2 
X = OneMinusRij[lower.tri(OneMinusRij)]

D = as.matrix( dist( USA_scaled )^2 )
Y = D[lower.tri(D)]

plot( X, Y )

summary( X/Y )

# (8)
## (a)
pr.out=prcomp(USArrests, scale=TRUE)
pr.var=pr.out$sdev^2
pve_1=pr.var/sum(pr.var)
pve_1

## (b)
USArrests_scaled = scale( USArrests )
bunbo = sum(apply( USArrests_scaled^2, 2, sum))
bunshi = apply( pr.out$x^2, 2, sum)
pve_2 = bunshi / bunbo



print(pve_1)
print(pve_2)
print(pve_1 - pve_2)


#(9)
## (a)
hc.nonscale=hclust(dist(USArrests), method="complete")

## (b)
plot(hc.nonscale,main="Complete Linkage", xlab="", sub="", cex=.9)
hc.out.nonscale = cutree(hc.nonscale, 3)

## (c)
USArrests_scaled = scale( USArrests )
hc.scale = hclust(dist(USArrests_scaled), method="complete")
hc.out.scale = cutree(hc.scale, 3)

## (d)
table(hc.out.nonscale, hc.out.scale)


# (10)
set.seed(0)
## (a)
n = 20
p = 50

X_1 = matrix(rnorm(n*p),n,p)
X_2 = X_1 + matrix(rnorm(n*p,0.5,0.5),n,p)
X_3 = X_1 - matrix(rnorm(n*p,0.5,0.5),n,p)

dat = rbind(X_1,X_2,X_3)
labels = c(rep(1,n),rep(2,n),rep(3,n))

## (b)
pr.out=prcomp(dat, scale=FALSE)
biplot(pr.out, scale=0)


plot( pr.out$x[,1], pr.out$x[,2], col=labels, pch=19 )

## (c)
km.out=kmeans(dat,3,nstart=20)
km.out$cluster
table(labels, km.out$cluster)

## (d)
km.out=kmeans(dat,2,nstart=20)
km.out$cluster
table(labels, km.out$cluster)

## (e)
km.out=kmeans(dat,4,nstart=20)
km.out$cluster
table(labels, km.out$cluster)

## (f)
dat.pr = rbind(pr.out$x[,1], pr.out$x[,2])
km.out=kmeans(dat,3,nstart=20)
table(labels, km.out$cluster)

## (g)
dat.scale = scale(dat)
km.out=kmeans(dat.scale,3,nstart=20)
table(labels, km.out$cluster)

# (11)
## (a)
dat = read.csv("Ch10Ex11.csv",header=F)
dat = t(dat)
## (b)
dd = as.dist(1-cor(t(dat)))
hc.complete=hclust(dd, method="complete")
hc.average=hclust(dd, method="average")
hc.single=hclust(dd, method="single")
table(hc.complete,hc.average)

## (c)
predicted=cutree( hc.complete, k=2 ) 

n1 = apply( dat[ predicted==1, ], 2, length ) 
n2 = apply( dat[ predicted==2, ], 2, length )

m1 = apply( dat[ predicted==1, ], 2, mean ) 
m2 = apply( dat[ predicted==2, ], 2, mean )

v1 = apply( dat[ predicted==1, ], 2, var ) 
v2 = apply( dat[ predicted==2, ], 2, var )

pooled_variance = sqrt( v1 / n1 + v2 / n2 )

t_value = ( m1 - m2 ) / pooled_variance 
plot(t_value)

### t検定
g1=v1/n1
g2=v2/n2
f = (g1+g2)^2/(g1^2/(n1-1)+g2^2/(n2-1))
print(f[which.min(t_value)])

## (c)_
pr.out=prcomp(dat, scale=TRUE)
plot( pr.out$x[,1], pr.out$x[,2], col=labels, pch=19 )
biplot(pr.out)

pr.var=pr.out$sdev^2
pve_1=pr.var/sum(pr.var)
pve_1
hc.complete=hclust(pr.out$rotation, method="complete")

matrix(1000*20,pr.out$rotation)







