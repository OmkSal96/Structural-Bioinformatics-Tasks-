
# if not installed yet: install.packages("rgl")
library(rgl)

# set working directory to directory with seminar material
setwd("F:/LECTURES/Structural_Bioinformatics/Seminar_3/SeminarMaterial/") 

f1=file("1zta_ca_1.xyz") # structure coordinate files
f2=file("1zta_ca_2.xyz")

# parsing the input:
f1_lines=strsplit(readLines(f1),"\n")
f2_lines=strsplit(readLines(f2),"\n")
close(f1)
close(f2)

n1=as.numeric(f1_lines[[1]])
n2=as.numeric(f2_lines[[1]])

m1=matrix(nrow=n1,ncol=3)
m2=matrix(nrow=n2,ncol=3)
l1=c()
l2=c()

for(i in 3:(n1+2))
{
  c=strsplit(f1_lines[[i]],"[\t ]+")
  l1=c(l1,c[[1]][1])
  m1[i-2,1]=c[[1]][2]
  m1[i-2,2]=c[[1]][3]
  m1[i-2,3]=c[[1]][4]
}

for(i in 3:(n2+2))
{
  c=strsplit(f2_lines[[i]],"[\t ]+")
  l2=c(l1,c[[1]][1])
  m2[i-2,1]=c[[1]][2]
  m2[i-2,2]=c[[1]][3]
  m2[i-2,3]=c[[1]][4]
}

# plotting structures:
par3d("windowRect"= c(50,50,600,600))

plot3d(m1,xlim=c(-20,20),ylim=c(-20,20),zlim=c(-20,20),box=F,axes=F,xlab="",ylab="",zlab="",col="red")
lines3d(m1,col="red")
text3d(m1,text=l1,col="red",cex=0.8)
plot3d(m2,col="green",add=T)
lines3d(m2,col="green")
text3d(m2,text=l1,col="green",cex=0.8)

# PCA = unit-length moments of inertia 
mn=matrix(as.numeric(m1),ncol=3,byrow=F)
p=prcomp(mn)

#moments of inertia
iV1=c(0.541059,-0.299595,0.785810)
iV2=c(0.787317,-0.103191,-0.581439)
iV3=c(0.010829,0.039588,0.007637)

#length=1
iV1n=iV1/sqrt(sum(iV1^2))
iV2n=iV2/sqrt(sum(iV2^2))
iV3n=iV3/sqrt(sum(iV3^2))

# PCA=inertia vectors but in different order, and possibly also signs (direction inverted)
