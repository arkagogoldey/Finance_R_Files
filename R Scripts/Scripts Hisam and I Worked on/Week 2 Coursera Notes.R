##Control Structures
#Controle structures in R allow you to control the flow of execution of the program, depending on runtime conditions. Common structures are

#if.else(): testing a condition

#for(): execute a loof a fixed number of times

#while(): execute a loof while a condition is true

#repeat(): execute an infinite loop

#break(): break the execution of a loop

#next(): skip an interation of a loop

#return(): exit a function

##If-else

if(<condition>){
	##do something
}
else
{
	##do something else
}
if(<condition>){
	#do something
}
else if(<condition2){
	#do something different
}
else{
	#do something different
}

###FOR EXAMPLE
x<-c(2)
if(x>3){
	y<- 10
}else{
	y<-0
}
y

#OR WE CAN DO

y<- if(x>3){
	10
}else{
	0
}

#For Loop
#for loops take an interator variable and assign it successive values from a sequence or vector. For loops are most commonly used for iterating over the elements of an object(list, vector,etc.)

for(i in 1:10){
	print(i)
}

#These three loops have the same behavior:
x<-c("a","b","c","d")
for(i in 1:4){
	print(x[i])
}

for(i in seq_along(x)){
	print(x[i])
}

for(letter in x){
	print(letter)
}

for(i in 1:4) print(x[i])
?replicate

##Nested for Loops
# for loops CAN be nested
x<-matrix(1:6,2,3)
for(i in seq_len(nrow(x))){
	for(j in seq_len(ncol(x))){
		print(x[i,j])
	}
}


##While loop, While loops begin by testing a condition. If it itis true, then they execute the loop body. Once the loop body is executed the condition is tested again, and so forth
#EX:
count<-0
while(count<10){
	print(count)
	count<-count+1
}
#While loops can potentially result in infinite loops if not written properly. Use with care!
#for loops are often safer because they have a hard limit

#Sometimes there will be more than one condition in the test
z<-5
while(z>=3 && z<=10){
	print(z)
	coin<-rbinom(1,1,.5)
	
	if(coin==1){##random walk
		z<-z+1	
	}else{
		z<-z-1
	}
}
#Conditions are always evaluated from left to right

##Repeat Loop
#Repeat initiates an infinite loop; these are not commonly used in statistical applications but they do have their uses. The only way to exit a repeat loop is to call break. 

x0<-1
tol<-1e-8
repeat{
	x1<-computeEstimate()##not a real function
	if(abs(x1-x0)<tol){
		break
	}else{
		x0<-x1
	}
}
#The loop is a bit dangerous because there's no guarantee it will stop. Better to set a hard limit on the number of iterations(e.g. using a for loop) and then report whether convergence was achieved or not.

##next and return
#next is used to skip an itteration of a loop
for(i in 1:100){
	if(i<=20){
		next
	}
	##do something to the remaining i's
}

#return signals that a function should exit and return a give value

###SUMMARY
#control structures like if, while, and for allow you to control the flow of an R program
#infinite loops should generally be avoided, even if they are theoretically correct.
##control structures mentioned here are primarily useful for writing programs; for command-line interactive work, the *apply functions are more useful. 

#Your First R Function!!(not really lol)
add2 <- function(x, y){
	x+y
}
add2(3,5)

##second vectors
above10<-function(x){
	use<- x > 10
	x[use]
}

above <- function( x, n=10){ #set a default value
	use<- x>n
	x[use]
}
x<-1:20
above(x)

colmean<- function(y, removeNA=TRUE){
	nc<- ncol(y)
	means <- numeric(nc)
	for (i in 1:nc){
		means[i]<- mean(y[,i],na.rm=removeNA)
	}
	means
}

colmean(airquality)

##Functions
#fnctions are created using the function() directive and are stored as R objects just like anything else. In particular, they are R objects of class "function".
f<-function(<arguments>){
	##do something interesting
}

##Functions in R are "first class objects," which means that they can be treated much like any other R object. Importantly,
	
	#Functions can be passed as arguments to other functions
	#Functions can be nested, so that you can define a function inside of another function. The reuturn value of a function is the last expression in the function body to be evaluated.

##Function arguments
#Functions have named arguments which potetialy have default values.
#the formal arguments are the arguments included in the function definition
#The formals function returns a list of all the formal arguments of a function
#Not every function call in R makes use of all the formal arguments
#Function arguments can be missing or might have defualt values

##Argument matching
#R functions arguments can be matched positionally, or by name. so the following calls to sd are all equivalent

mydata<- rnorm(100)
sd(mydata)
sd(x=mydata)
sd(x=mydata,na.rm=FALSE)
sd(na.rm=FALSE,x=mydata)
sd(na.rm=FALSE,mydata)
#Even though its legal, I dont recommend messing around with the order of the arguments too much, since it can lead to confusion

##Argument matching
#you can mix positional matching with matching by name. when an argument is matched by name, it is "taken out" of the argument list and the remaining unnamed arguments are matched in the order that they are listed in the function definition.

args(lm)
#most of the time, named arguments are useful on the command line when you have a long argument list and you want to use the defaults for everythign excrept for an argument near the end of the list.

#Named arguments also help if you can remember the name of the argument and not its position on the argument list (ploting is a good example).

#Function arguments can also be partially matched, which is useful for interactive work. The order of operations when given an argument is 
	#check for the exact match for a named argument
	#check for a partial match
	#check for a positional match
	
##Defining a function
f<-function(a,b=1,c=2,d=NULL)#Null means nothing there
{
	#do something
}

#Lazy Evaluation: Arguments to a function are evaluated lazily, so they are evaluated only as needed.
f<-function(a,b){
	a^2
}
f(2)

#The function never actually uses the argument b, so calling f(2) will not produce an error because the 2 gets positionally matched to a.

f<-function(a,b){
	print(a)
	print(b)
}
f(45)
#Notice that "45" got printed first before the error was triggered. this is because b did not have to be evealuated until after print(a). Once the function tried to evaluate print(b) it had to throw an error.

#The"..." argument
#The ... argument indicate a variable of arguments that are usually passed on to the other functions. 
#... is often used when extending another function and you don't want to copy the entire argument list of the original function
myplot<-function(x,y,type="l",...){
	plot(x,y,type=type,...)
} 

#Generic function use ... so that extra arguments can be passed to methods (more on this later).

#The ... argument is also necessary when the number of arguments passed to the function cannot be known in advance

args(paste) #unkown number of vectors to be put together

args(cat) #unknown number of vectors to be concatenated.

#one catch with... is that any arguments that appear after .... on the argument list must be named EXPLICITYLY and cannot be partially matched.

###LEXICAL SCOPING
#why does all this matter?
	#typically, a function is defined in the global enviornment, so that the values of free variables are just found in the user's workspace
	#This behavior is logical for most people and is usually the "right thing" to do
	#However, in R you can have functions defined inside other functions
		#languages like C don't let you do this
	#Now things get interesting - In this case the enviornment in which a function is defined is the body of another function!
#EXample:

make.power<-function(n){
	pow<-function(x){
		x^n
	}
	pow
}
cube<-make.power(3)
square<-make.power(2)
cube(3)
square(3)
#cool

##exploring a function closure
#what's in a functions enviornment?
ls(environment(cube))
get("n", environment(cube))
ls(environment(square))
get("n",environment(square))

#Lexical vs. Dynamic Scoping
y<-10
f<-function(x){
	y<-2
	y^2 + g(x)
}

g<-function(x){
	x*y
}
f(3)

#With lexical scoping the value of y in the function g is looked up in the environment in which the function was defined, in this case the global environment, so the value of y is 10

#with dynamic scoping, the value of y is looked up in the environment from which the function was called (someimes referred to as the calling environment).
	#In R the calling environment is known as the parent frame
#So the value of y would be 2

#When a function is defined in the global environemnt and is subsequently called from the global env, then the defining env and the calling env are the same. This can sometimes give the appearance of dynamic scoping.
rm(list=ls())
g<-function(x){
	a<-3
	x+a+y
}
g(2)
##object y not found
y<-3
g(2)
#8


##other languanges that support lexical scoping
	#Scheme
	#Perl
	#Python
	#Common Lisp (all languages converge to Lisp)

##Consequences of Lexical scoping
#in R, all objects must be stored in memory
#All functions must carry a pointer to their respective defining environments, which could be anywhere,
#In S-Plus, free variables are always looked up in the global workspace, so everything can be stored on the disk because the "defining environment" of all functions is the same.

##Application: Optimization
#Why is any of this information useful?
	#Optimization routines in R like optim(), nlm(), and optimize() require you to pass a function whose argument is a vector of parameters (e.g. a log-likelihood)
	#However, an object function might depend on a host of other things besides its paramters (like data)
	#When writing software which does optimzation, it may be desiable to all the use to hold certain parameters fixed.

#Maximizing a Normal likelihood

#Write a "constructor" function
make.NegLogLik<-function(data, fixed=c( FALSE ,FALSE)){
	params<- fixed
	function(p){
		params[!fixed]<-p
		mu<-params[1]
		sigma<-params[2]
		a<- -.5*length(data)*log(2*pi*sigma^2)
		b<- -.5*sum((data-mu)^2)/(sigma^2)
		-(a+b)
	}
}

set.seed(1);normals<-rnorm(100,1,2)
nLL<-make.NegLogLik(normals)
nLL

ls(environment(nLL))

optim(c(mu=0,sigma=1),nLL)$par
#Fixing sigma =2
nLL<-make.NegLogLik(normals,c(FALSE,2))
optimize(nLL,c(-1,3))$minimum

##Fixing mu=1
nLL<-make.NegLogLik(normals,c(1,FALSE))
optimize(nLL,c(1e-6,10))$minimum

#Plotting the likelihood
nLL<-make.NegLogLik(normals,c(1,FALSE))
x<-seq(1.7,1.9,len=100)
y<-sapply(x,nLL)
plot(x,exp(-(y-min(y))),typ="l")

nLL<-make.NegLogLik(normals,c(FALSE,2))
x<-seq(0.5,1.5,len=100)
y<-sapply(x,nLL)
plot(x,exp(-(y-min(y))),typ="l")

#Objective function can be "built" which contain all of the neccesary data for evaluating the function 
#No need to carry around long argument lists- useful for interactive an explorartoy work
#code can be simplified and cleaned up
#Reference: Robert Gentleman and Ross Ihaka(2000). "Lexical scope and statiscal computing." JCGS,9,491-508

###Coding standards
#Always use text files/text editor
#Indent your code (4-8 spaces)
#limit the width of your code (80 columns?)
#Limit the length of individual functions
	#can help with reading
	#help with debugging
	
	
##Dates and Times in R
#R has developed a special representation of dates and times
	#Dates are represented by the Date Class
	#Times are represented by the POSIXct or POSIXlt class
	#Dates are stored internally as the number of days since 1970-01-01
	#Times are stored internally as the number of seconds since 1970-01-01

#Dates are represented by the Date calss and can be coereced from a character string using the as.Date() function

x<-as.Date("1970-01-01")
x

unclass(x)
unclass(as.Date("1970-01-02"))

#Times are represented by using the POSIXct or POSIXlt class
	#POSIXct is just a very large integer under the food; it use a useful class when you want to store times in something like a data frame
	#POSIXlt is a list underneath and it storesa bunch of other useful information like the day of the week, day of the year, month, day of the month

#there are a number of generic functions that work on dates and times
	#weekdays(): give the day of the week
	#months(): give the month name
	#quarters(): gives the quarter number
	
x<-Sys.time()
x
p<-as.POSIXlt(x)
names(unclass(p))
p$sec

#Finally, there is the strptime function in case you dates are written in a different format
datestring<-c("January 10, 2013 10:40","December 9, 2011 9:10")
x<-strptime(datestring,"%B %d,%Y %H:%M")
x
##operations on dates and times
y<-as.Date("2012-01-01")
y<-as.POSIXlt(y)
##gets error need to make them the same class
x<-as.POSIXlt(x)
y-x

##Summary
#Dates and times have special classes in R that allow for numerical and stats calcultions
#Datesa use the Date class
#Time use the POSIXct and POSIXlt class
#Character stings can be coerced to Date/Time clases using the strptime function or the as.Date, as.POISXlt, or as.POISXct
