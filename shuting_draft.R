library(R.matlab)
binned_zscore <- readMat("Data/binned_zscore.mat")
binned_zscore <- binned_zscore$binned.zscore
dim(binned.zscore) ##6300*47
binned_zscore2 <- readMat("Data/binned_zscore-2.mat")
binned_zscore2 <- binned_zscore2$binned.zscore
dim(binned_zscore2) ##6301*128

binned_behavior <- readMat("Data/binned_behavior.mat")
binned_behavior <- binned_behavior$binned_behavior
dim(binned_behavior) ##2*6300
binned_behavior2 <- readMat("Data/binned_behavior-2.mat")
binned_behavior2 <- binned_behavior2$binned.behavior
dim(binned_behavior2) ##2*6301

########eda.rmd
length(folders) ##13 files
## bb file & bz file are pairs. So there are 13 pairs.
## for each pair, their dims are different. So I guess each pair stands for one single mouse.
## violin plot can see the median value of all neurons, compare close and open level

## QUESTION IS: do we need to remove the WIRED data (0,0)?
st <- z ##check wired data
st$check <- st[,33]+st[,34]
length(which(st$check==0))##237 0&0
length(which(st$check==2))##there is no 1&1
(st %>% subset((st$check==0)))$time

## first thought: plot time series for each neurons
for (i in 1:32){index[i] <- paste0("X",i)}
ggplot(z_long[z_long$name %in% index[1:10],])+
  aes(x=time,y=value,group=name,color=name)+
  geom_line()+
  facet_wrap(~name,nrow=2)+
  xlab("time")+ylab("activity level")+labs(color="neurons")

## 