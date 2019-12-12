
library(ggplot2)
library(reshape2)
# N is the non-mutant shown in red labeled drug sensitive
# M is the mutant shown in blue labeled drug resistant


days=500
r=0.1
rm=0.05
rn=-0.1
K=1000000
N0=1
M0=1
N=numeric(length=days)
N[1]=N0
M=numeric(length=days)
M[1]=M0

for(t in 1:(days-1)){
  if(t<210){
    if(N[t]>99){
      N[t+1]=N[t]+r*N[t]*(1-((N[t]+M[t])/K))
      M[t+1]=M[t]+r*M[t]*(1-((N[t]+M[t])/K))
    }else {
      N[t+1]=N[t]+r*N[t]*(1-(N[t]/K))
      M[t+1]=1
    } 
  }else{
    N[t+1]=N[t]+rn*N[t]*(1-((N[t]+M[t])/K))
    M[t+1]=M[t]+rm*M[t]*(1-((N[t]+M[t])/K))
  }
}

simEvents<-data.frame(time=1:length(N), Drug_sensitive_cells=N, Drug_resistant_cells=M)
test_data_long <- melt(simEvents, id="time")
print(test_data_long)
ggplot(data=test_data_long, aes(x=time, y=value, colour=variable))+
  geom_line()+
  ylab("Number of Cells")+
  xlab("Time (Days)")

