regday2.pck <-
c("regday2.pck", "gauss.reg", "gauss.reg1a", "my.hat.w1", "gen.inv.sing", 
"cv.train.test", "cv.train.test0", "kfold.cv", "cv.press", "Fun.in.Tstats", 
"matrix.2ndorder.make")
gauss.reg <-
function(x,y,lambda,xcol=4,do.plot=T)
{
o1<-order(x)
x1<-x[o1]
y1<-y[o1]
r1<-range(x)
smat<-NULL
n1<-length(x1)
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda)
        v1<-v1/sum(v1)
        doh<-my.hat.w1(x1,v1)
	H1<-doh$ret0
	#print(doh$sing)
        smat<-rbind(smat,H1[i,])
}
yhat<-smat%*%y1
if(do.plot){
lines(x1,yhat,col=xcol)
}
n99<-length(x1)
dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
delta2<-2*sum(diag(R%*%R))
resid<-y1-smat%*%y1
ypred<-y1
ypred[o1]<-smat%*%y1
PRESS<-sum((resid/(1-diag(smat)))^2)
list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred=ypred,lambda=lambda,PRESS=PRESS)
}
gauss.reg1a <-
function(x,y,lambda,xcol=4)
{
o1<-order(x)
x1<-x[o1]
y1<-y[o1]
r1<-range(x)
smat<-NULL
n1<-length(x1)
lambda1<-lambda
sing0<-F
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda)
        v1<-v1/sum(v1)
        retlist<-my.hat.w1(x1,v1)
	  H1<-retlist$ret0
	  if(retlist$sing){
		sing0<-T
	  }
        smat<-rbind(smat,H1[i,])
		
}
kk<-0
while(sing0){
kk<-kk+1
#print(kk)
lambda1<-lambda1*2.3
#print(lambda1)
smat<-NULL
n1<-length(x1)
sing0<-F
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda1)
        v1<-v1/sum(v1)
        retlist<-my.hat.w1(x1,v1)
	  H1<-retlist$ret0
#print(retlist$sing)
	  if(retlist$sing){
		sing0<-T
	}
        smat<-rbind(smat,H1[i,])
		
}
}
lambda0<-lambda1
resid<-y1-smat%*%y1
Press0<-sum((resid/(1-diag(smat)))^2)
lambda0a<-lambda0*1.05
Press0a<-gauss.reg(x,y,lambda0a,do.plot=F)$PRESS
lambda0b<-lambda0*1.1
Press0b<-gauss.reg(x,y,lambda0b,do.plot=F)$PRESS
Py<-c(Press0,Press0a,Press0b)
xlamb<-c(lambda0,lambda0a,lambda0b)
xL<-cbind(xlamb,xlamb^2)
print(xL)
print(Py)
c1<-lsfit(xL,Py)$coef
lambdatarg<-(-c1[2]/(2*c1[3]))
lambvec<-lambda0+c(1:15)/10*(lambdatarg-lambda0)
lambvec<-sort(c(lambvec,xlamb))
Pressvec<-NULL
for(lamb in lambvec){
Pressvec<-c(Pressvec,gauss.reg(x,y,lamb,do.plot=F)$PRESS)
}
plot(lambvec,Pressvec)
Imin<-Pressvec==min(Pressvec)
lambda00<-lambvec[Imin][1]
plot(x,y)
gauss.reg(x,y,lambda00,do.plot=T)
}
my.hat.w1 <-
function(x,wt){
x1<-cbind(1,x)
sing<-gen.inv.sing(t(x1)%*%diag(wt)%*%x1)
ret1<-x1%*%gen.inv1(t(x1)%*%diag(wt)%*%x1)%*%t(x1)%*%(diag(wt))
list(sing=sing,ret0=ret1)
}
gen.inv.sing <-
function(mat, thresh = 1e-16)
{
	v1 <- sum(is.na(mat))
	v2 <- sum(is.inf(mat))
	if((v1 + v2) > 0.5) {
		print(mat)
	}
	e1 <- eigen(mat, symmetric = T)
	val <- Re(e1$val)
	vec <- Re(e1$vec)
	val1 <- val/max(val)
	#
	#	print("normalized eigen values")
	#	print(val1)	#
	#	n1 <- length(val1)
	#	plot(c(1:n1), abs(val1), log = "y", xlab = "eigen rank", ylab
	#		 = "log10 of value")
	I1 <- val1 > thresh
#print(I1)
	I3 <- is.na(I1)
	sing<-F
	if(sum(I1) < length(I1)) {
		sing<-T
		
	}
	sing
}
cv.train.test <-
function(xmat,ystring,xstring,prop.test)
{
n1<-length(xmat[,1])   #How many data points are there
n2<-floor(n1*prop.test) #How many in prop.test proportion of the data
test.ID<-sample(n1,n2) # randomly sample that proportion
trainmat<-xmat[-test.ID,] #create the training matrix by removing that randomly sampled set
testmat<-xmat[test.ID,] #create the test matrix as the randomly sampled set
lm.str<-lm(as.formula(paste(ystring,"~",xstring)),trainmat,x=T,y=T) #paste the three strings "ystring","~", 
#and "xstring" together and evaluate them as the formula inside LM, fit train.mat. Return the original
# x and y as part of the output
ypred<-predict(lm.str,testmat) # the model fit to the training data to predict the test data
I1<-attributes(xmat)$names==ystring # identify the y variable position in the names vector of the data frame
ny<-c(1:length(I1))[I1] # change that position to a number
testy<-testmat[[ny]] #extract y from the testmat
par(mfrow=c(1,2)) # set up a plot region with 2 plots in a row
plot(lm.str$fitted.values,lm.str$y,xlab="fit",ylab="values",main="Training data") #Plot the fit to the training data
lines(lm.str$fitted.values,lm.str$fitted.values) # add a line showing what perfect would be.
plot(ypred,testy,xlab="prediction",ylab="values",main="Testing data") #plot the prediction of the test data
lines(ypred,ypred)  #add a lline showing what perfect would be.
}
cv.train.test0 <-
function(xmat,ystring,xstring,test.ID)
{
trainmat<-xmat[-test.ID,]
testmat<-xmat[test.ID,]
lm.str<-lm(as.formula(paste(ystring,"~",xstring)),trainmat,x=T,y=T)
ypred<-predict(lm.str,testmat)
I1<-attributes(xmat)$names==ystring
ny<-c(1:length(I1))[I1]
testy<-testmat[[ny]]
yfull<-c(lm.str$y,testy)
ss0<-sum((yfull-mean(yfull))^2)
list(pred=ypred,y=testy,ss=ss0)
}
kfold.cv <-
function(xmat,ystring,xstring,k,do.plot=T)
{
prop.test<-1/k
n1<-length(xmat[,1])
n2<-floor(n1*prop.test)
base.sample<-sample(n1) # this gives a random permutation of  the vector 1:n1
par(mfrow=c(2,k/2))
ss<-0
ss0<-sum(lm(as.formula(paste(ystring,"~",xstring)),xmat)$resid^2) # WHAT IS $resid
for(i in 1:k){ #This is a loop structure it takes i in order from the vector 1:k. and applies it to the algorithm in brackets WHY DO IT?
svec<-(i-1)*n2+c(1:n2) # this chooses the section of basevec, hence what part of the data frame is the test sample
strout<-cv.train.test0(xmat,ystring,xstring,base.sample[svec]) #Hey code reuse!
ss<-ss+sum((strout$y-strout$pred)^2)
if(do.plot){#if the statement in parantheses is true it does what is in brackets following, otherwise it skips it 
plot(strout$pred,strout$y,xlab="prediction",ylab="actual")
lines(strout$pred,strout$pred)
} # the if statement ends here
} # the loop ends here
list(ss0=ss0,ss=ss)
}
cv.press <-
function(xmat,ystring,xstring)
{
lm.str<-lm(as.formula(paste(ystring,"~",xstring)),xmat,x=T,y=T)
special.resid<-lm.str$resid/(1-hat(lm.str$x))
list(leave1out.resid=special.resid,leverage=hat(lm.str$x),PRESS=sum(special.resid^2))
}
Fun.in.Tstats <-
function(Bx=.1,By=1,n=50){
        x1<-rnorm(n)
        x2<-(x1*Bx+rnorm(n))
        x2<-x2/(sd(x2))
        y<-x1+x2+rnorm(n)+1
        par(mfrow=c(2,2))
        plot(x1,x2,main=paste("cor=",cor(x1,x2)))
        lstr<-lsfit(cbind(x1,x2),y)
        print(lstr$coef)
        yhat<-lstr$coef[1]+cbind(x1,x2)%*%c(lstr$coef[-1])
        mse<-sum(lstr$resid^2)/(length(y)-3)
        plot(yhat,y)
        plot(yhat,lstr$resid)
        ls.print((lstr))
        Hat=hat(cbind(x1,x2))
        xmat<-cbind(x1,x2,1)
        xmat1<-cbind(x1,x2)
        xtxinv<-solve(t(xmat)%*%xmat)
        
        list(Hat=Hat,xtx=t(xmat)%*%xmat,covBnos=xtxinv,mse=mse,cor.xmat=cor(xmat1),corinv=solve(cor(xmat1)),eigen=eigen(cor(xmat1)))    
}
matrix.2ndorder.make <-
function(x, only.quad=F){
x0<-x
dimn<-dimnames(x)[[2]] #extract the names of the variables
num.col<-length(x[1,]) # how many columns
for(i in 1:num.col){
# if we are doing all 2nd order
if(!only.quad){
for(j in i:num.col){
x0<-cbind(x0,x[,i]*x[,j])
dimn<-c(dimn,paste(dimn[i],dimn[j],sep=""))
#create interaction dimnames

}
}
else{
#in here only if doing only squared terms
x0<-cbind(x0,x[,i]*x[,i])
dimn<-c(dimn,paste(dimn[i],"2",sep="")) # squared dimmension names
}
}
dimnames(x0)[[2]]<-dimn
x0
}
