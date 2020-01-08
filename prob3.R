fgam<-function(z){
  foo<-function(x){x**(z-1)*exp(1)**(-x)}
  if (z > 0){
      return(integrate(foo, lower = 0, upper = Inf))
  }else
  {
      z1=(z-1)*(-1)
      return(pi/(sin(pi*z1)*(fgam(z1)$value)))
  }
}

print(fgam(-3/2))
print(fgam(1/2))
print(fgam(3))
print(fgam(1))


fbet<-function(a,b){
   return(((fgam(a)$value)*(fgam(b)$value))/(fgam(a+b)$value))
}

fbetinc<-function(x,a,b){
  
  return(integrate(function(t) t**(a-1)*(1-t)**(b-1), lower = 0, upper = x))
}

print(fbet(1/2,1/2))
print(fbet(1/3,1/2))
print(fbet(1/2,1/3))


fdensgamma<-function(x,a,b){
  if (a > 0 && b > 0 && x > 0)return((1/((b**a)*fgam(a)$value))*(x**(a-1))*exp(1)**(-x/b))
}
fdensbeta<-function(x,a,b){
  if (a > 0 && b > 0 && x > 0 && x < 1)return((1/(fbet(a,b)))*(x**(a-1))*(1-x)**(b-1))
}
fprobgamma<-function(x,a,b){
  if (x > 0){
      fint<-function(t) fdensgamma(t,a,b)
      return(integrate(fint, lower = 0, upper = x))
  }
}


print("Punctul c)")
print(fprobgamma(3,1/2,1/2))

print((fprobgamma(5,1/2,1/2)$value - fprobgamma(2,1/2,1/2)$value))

print(fprobgamma(4,1/2,1/2)$value - fprobgamma(3,1/2,1/2)$value)

print("4) P(Y > 2) non finite? fbetainc(2,1/2,1/2) ar returna complex?

sau print(1 - fbetinc(as.complex(2),1/2,1/2)$value/fbet(1/2,1/2))?")

print(fprobgamma(6,1/2,1/2)$value - fprobgamma(4,1/2,1/2)$value)

print(fprobgamma(1,1/2,1/2)$value - fprobgamma(0,1/2,1/2)$value)

p<-0

p<-0 + (fprobgamma(4,1/2,1/2)$value - fprobgamma(-Inf,1/2,1/2)$value)*fbet(1/2,1/2) + (fprobgamma(5,1/2,1/2)$value-fprobgamma(4,1/2,1/2)$value)*fbet(1/2,1/2)/2

print(p);

p<-0

p<-0 + (fprobgamma(Inf,1/2,1/2)$value - fprobgamma(0.5,1/2,1/2)$value)*fbet(1/2,1/2) + (fprobgamma(0.5,1/2,1/2)$value-fprobgamma(-0.5,1/2,1/2)$value)*fbet(1/2,1/2)/2

print(p)

p<-0

p<-0 + (fprobgamma(Inf,1/2,1/2)$value - fprobgamma(3,1/2,1/2)$value)*fbet(1/2,1/2) + (fprobgamma(3,1/2,1/2)$value-fprobgamma(2,1/2,1/2)$value)*fbet(1/2,1/2)/2

print(p)
print("Punctul d)")
print(pgamma(3,1/2,1/2))

print(pgamma(5,1/2,1/2) - pgamma(2,1/2,1/2))

print(pgamma(4,1/2,1/2) - pgamma(3,1/2,1/2))

print(pbeta(2,1/2,1/2,lower.tail = FALSE))
print(pgamma(6,1/2,1/2) - pgamma(4,1/2,1/2))
print(pgamma(1,1/2,1/2) - pgamma(0,1/2,1/2))
print((pgamma(4,1/2,1/2) - pgamma(-Inf,1/2,1/2))*beta(1/2,1/2) + (pgamma(5,1/2,1/2) - pgamma(4,1/2,1/2))*beta(1/2,1/2)/2)
print((pgamma(Inf,1/2,1/2) - pgamma(0.5,1/2,1/2))*beta(1/2,1/2) + (pgamma(0.5,1/2,1/2) - pgamma(-0.5,1/2,1/2))*beta(1/2,1/2)/2)
print((pgamma(Inf,1/2,1/2) - pgamma(3,1/2,1/2))*beta(1/2,1/2) + (pgamma(3,1/2,1/2) - pgamma(2,1/2,1/2))*beta(1/2,1/2)/2)
