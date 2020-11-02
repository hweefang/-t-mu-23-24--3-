mu=-3
#SRG parameters
miu_array=c(85, 14, 3, 2, 4, 45, 12, 3, 3, 3, 231, 30, 9, 3, 9, 26, 10, 3, 4, 2, 126, 25, 8, 4, 7, 69, 20, 7, 5, 5, 25, 12, 5, 6, 2, 36, 15, 6, 6, 3, 49, 18, 7, 6, 4, 64, 21, 8, 6, 5, 81, 24, 9, 6, 6, 100, 27, 10, 6, 7, 121, 30, 11, 6, 8, 144, 33, 12, 6, 9, 169, 36, 13, 6, 10, 196, 39, 14, 6, 11, 225, 42, 15, 6, 12, 256, 45, 16, 6, 13, 289, 48, 17, 6, 14, 324, 51, 18, 6, 15, 361, 54, 19, 6, 16, 400, 57, 20, 6, 17, 441, 60, 21, 6, 18, 484, 63, 22, 6, 19, 529, 66, 23, 6, 20, 26, 15, 8, 9, 2, 35, 18, 9, 9, 3, 57, 24, 11, 9, 5, 70, 27, 12, 9, 6, 100, 33, 14, 9, 8, 117, 36, 15, 9, 9, 155, 42, 17, 9, 11, 176, 45, 18, 9, 12, 222, 51, 20, 9, 14, 247, 54, 21, 9, 15, 301, 60, 23, 9, 17, 330, 63, 24, 9, 18, 392, 69, 26, 9, 20, 425, 72, 27, 9, 21, 495, 78, 29, 9, 23, 532, 81, 30, 9, 24, 610, 87, 32, 9, 26, 651, 90, 33, 9, 27, 737, 96, 35, 9, 29, 36, 21, 12, 12, 3, 441, 88, 35, 13, 25, 76, 35, 18, 14, 7, 99, 42, 21, 15, 9, 189, 60, 27, 15, 15, 575, 112, 45, 16, 32, 40, 27, 18, 18, 3, 96, 45, 24, 18, 9, 49, 32, 21, 20, 4, 232, 77, 36, 20, 19, 75, 42, 25, 21, 7, 261, 84, 39, 21, 21, 375, 102, 45, 21, 27, 105, 52, 29, 22, 10, 76, 45, 28, 24, 7, 126, 60, 33, 24, 12, 162, 69, 36, 24, 15, 1344, 221, 88, 26, 65, 95, 54, 33, 27, 9, 196, 81, 42, 27, 18, 1911, 270, 105, 27, 81, 476, 133, 60, 28, 35, 57, 42, 31, 30, 4, 64, 45, 32, 30, 5, 96, 57, 36, 30, 9, 288, 105, 52, 30, 25, 540, 147, 66, 30, 39, 225, 96, 51, 33, 21, 405, 132, 63, 33, 33, 176, 85, 48, 34, 17, 703, 182, 81, 35, 49, 841, 200, 87, 35, 55, 50, 42, 35, 36, 2, 56, 45, 36, 36, 3, 76, 54, 39, 36, 6, 125, 72, 45, 36, 12, 154, 81, 48, 36, 15, 300, 117, 60, 36, 27, 550, 162, 75, 36, 42, 126, 75, 48, 39, 12, 81, 60, 45, 42, 6, 120, 77, 52, 44, 11, 351, 140, 73, 44, 32, 77, 60, 47, 45, 5, 105, 72, 51, 45, 9, 175, 102, 65, 51, 17, 112, 81, 60, 54, 9, 176, 105, 68, 54, 17, 276, 135, 78, 54, 27, 100, 77, 60, 56, 7, 162, 105, 72, 60, 15, 243, 132, 81, 60, 24, 253, 140, 87, 65, 25,275, 162, 105, 81, 27)

t=24 # t=23
n=0.5*t*(t+1)
mat_miu=matrix(miu_array,ncol=5,byrow=T)
data_miu=data.frame(mat_miu)
names(data_miu)=c('Ni','kii','ei','fi','lambdai')
ni= data_miu[,'Ni'] #vector of ni
k=data_miu[,'kii']
f=data_miu[,'fi']

#generate vector of unique ni
v=NULL #empty matrix or vector
#Inf
for(n in (0.5*t*(t+1))){
  for(i in 1:(length(ni))){
    for(j in (i):length(ni)){
      if(ni[i]+ni[j]==n){
        if(ni[i]%in%v){
          if(ni[j]%in%v){}
          else{v=c(v,ni[j])}
        }else(
          if(ni[j]%in%v){v=c(v,ni[i])}
          else{v=c(v,c(ni[i],ni[j]))})
      }
    }
  }
}

v #unique ni

#subset of mat_miu
subset_dat=NULL
for(i in v){subset_dat=rbind(subset_dat,subset(data_miu,Ni==i))}
subset_dat
dim(subset_dat)

#add a new dataframe with n1,n2
v1=subset_dat$Ni
len=length(v1)
subset_dat1=NULL
for(i in 1:(len-1)){
  for(j in (i+1):len){
    if(v1[i]+v1[j]==n){subset_dat1=rbind(subset_dat1,c(subset_dat$Ni[i],subset_dat$Ni[j],subset_dat$kii[i],subset_dat$kii[j],subset_dat$ei[i],subset_dat$ei[j],subset_dat$fi[i],subset_dat$fi[j],subset_dat$lambdai[i],subset_dat$lambdai[j]))}
  }
} 
subset_dat1
colnames(subset_dat1)=c('n1','n2','k11','k22','e1','e2','f1','f2','lambda1','lambda2')
subset_dat1

#add lambda
lambda=subset_dat1[,'lambda1']+subset_dat1[,'lambda2']-mu
subset=cbind(subset_dat1,lambda)
subset

#add p
p=-( (t-1)*lambda+0.5*t*(t-1)*mu)
subset=cbind(subset,p)
subset

#check p+mu==k11+k22
check=(p+mu==subset[,'k11']+subset[,'k22'])
subset=cbind(subset,check)
#zero if contradiction holds
#check it is a valid SRG

k21=sqrt((subset_dat1[,'k11']*subset_dat1[,'k22']-p*mu)*subset_dat1[,'n1']/subset_dat1[,'n2'])
subset=cbind(subset,k21)
subset