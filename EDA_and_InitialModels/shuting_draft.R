#binned_zscore <- readMat("Data/binned_zscore.mat") ##6300*47
# binned_zscore2 <- readMat("Data/binned_zscore-2.mat") ##6301*128
# binned_behavior <- readMat("Data/binned_behavior.mat")##2*6300
# binned_behavior2 <- readMat("Data/binned_behavior-2.mat")##2*6301

########eda.rmd###############################
length(folders) ##13 files
## bb file & bz file are pairs. So there are 13 pairs.
## for each pair, their dims are different. So I guess each pair stands for one single mouse.
## violin plot can see the median value of all neurons, compare close and open level
###### read data
prefix <- "Data/Zero_Maze/"
folders <- list.files(prefix)
for (f in folders){
#  f <- "616669_251"
  f_abbr <- str_sub(f, -3, -1)
  sub_f <- list.files(paste0(prefix, f, "/Day_1/Trial_001_0"))
  assign(paste0("bb_",f_abbr), 
          readMat(paste0(prefix, f, "/Day_1/Trial_001_0/binned_behavior.mat"))$binned.behavior)
  assign(paste0("bz_",f_abbr),
         readMat(paste0(prefix, f, "/Day_1/Trial_001_0/binned_zscore.mat"))$binned.zscore)
}

##### For example, mouse 251
bb_251 <- data.frame(t(bb_251))
bz_251 <- data.frame(bz_251)
bz_251$open <- bb_251[,2]
bz_251$closed <- bb_251[,1]
bz_251$time <- seq(1:nrow(bz_251))
bz_251_long <- bz_251 %>% pivot_longer(cols = -c(open, closed, time))
## drop wired data & combined closed and open into 1 column
bz_251_long_clean <- bz_251_long[-which(bz_251_long$open==0 & bz_251_long$closed==0),]
bz_251_long_clean$Y <- NA
bz_251_long_clean$Y[bz_251_long_clean$open==1] <- "open"
bz_251_long_clean$Y[bz_251_long_clean$closed==1] <- "closed"

## first thought: plot time series for each neurons, split by open/closed
index <- NULL
for (i in 1:32){index[i] <- paste0("X",i)}
ggplot(bz_251_long_clean[bz_251_long_clean$name %in% index[1:10],])+
  aes(x=time,y=value,group=name,color=name)+
  geom_line()+
  facet_wrap(Y~name,nrow=2)+
  xlab("time index")+ylab("zscore")+labs(color="neurons")

## plot mean level for close and open, dropped wired data
bz_251_long_clean_temp <- bz_251_long_clean[,3:ncol(bz_251_long_clean)] %>% group_by(Y,name) %>% summarise(avg=mean(value)) %>% arrange(name)

ggplot(bz_251_long_clean_temp)+
  geom_bar(aes(x=name,y=avg,fill=Y),stat = "identity",position="dodge")+
  xlab("neurons")+ylab("average zscore")+labs(fill="behavior",title="mouse 251 in Zero_Maze experiment")
  
#################### Fit Simple Model ##################

#step1: dimension reduction
#use baseline_models.Rmd's mdl_data
library(FactoMineR)
library(factoextra)
mdl_data.pca <- PCA(mdl_data[,-1], scale.unit = TRUE, ncp=5, graph = FALSE)
get_eigenvalue(mdl_data.pca) #80:95.6%; 60:90%; 40: 80.8%
fviz_eig(mdl_data.pca) #visualize contribution
var <- get_pca_var(mdl_data.pca) #contrib is contribution of pc
ind <- get_pca_ind(mdl_data.pca) ##what's the difference?
fviz_contrib(mdl_data.pca, choice = "var", axes = 1, top = 10) #visualize contribution to pc1
#pca <- prcomp(mdl_data[,-1], scale. = TRUE) #same with PCA()

#step2: fit logistic model
comp <- data.frame(mdl_data.pca$ind$coord)
comp$open <- mdl_data$open
train <- sample(1:nrow(comp), size = round(.8*nrow(comp)), replace = FALSE)
training <- comp[train,]
testing <- comp[-train,]
mdl_logis <- glm(open~., data = training, family = binomial("logit"))
summary(mdl_logis)
pred_logis <- predict(mdl_logis, newdata = testing)
error_rate <- mean((pred_logis>.5 & testing$open == 0) | (pred_logis<.5 & testing$open == 1))
error_rate
## visualize: confusion matrix, ROC curve ##
library(pROC)
logisROC <- roc(testing$open, pred_logis)
plot(logisROC, print.auc=TRUE, auc.polygen=TRUE)

######PCA+RNN####
dim(comp)
n_lags <- 5
epochs <- 100
#data_mat <- data.matrix(data_clean %>% select(-closed))
xdata <- data.matrix(comp %>% select(-c(open)))
ydata <- data.matrix(comp %>% select(open))
#create lag function
lagm <- function (x, k = 1) {
  n <- nrow (x)
  pad <- matrix (NA , k, ncol (x))
  rbind (pad , x[1:(n - k), ])
}
#make lags
arframe <- data.frame ( open = ydata ,
                        L1 = lagm ( xdata , 1) , L2 = lagm ( xdata , 2),
                        L3 = lagm ( xdata , 3) , L4 = lagm ( xdata , 4) ,
                        L5 = lagm ( xdata , 5)
)
#separate train and test
istrain <- rep(TRUE, round(nrow(arframe)*0.7))
istrain <- c(istrain, rep(FALSE,nrow(arframe)-round(nrow(arframe)*0.7)))
#remove na rows due to lags
arframe <- arframe [ -(1:n_lags) , ]
istrain <- istrain [ -(1:n_lags) ]
ydata <- ydata[-(1:n_lags)]
n <- nrow ( arframe )
#select only neuron data, including lags -- exclude "open"
xrnn <- data.matrix ( arframe [ , -1])
dim(xrnn)
xrnn <- array ( xrnn , c (n , 5 , n_lags) )  #format to n rows; number of neurons columns; number of lags layers
dim(xrnn)
xrnn <- xrnn [ , , n_lags:1] #reorder columns 
#aperm = Transpose an array by permuting its dimensions and optionally resizing it.
#he final step rearranges the coordinates of the array (like a partial transpose) into the format that the RNN module in keras expects
xrnn <- aperm ( xrnn , c (1 , 3 , 2) )
dim ( xrnn )
#install_keras()
library(keras)
library(tensorflow)
model <- keras_model_sequential () %>%
  layer_simple_rnn ( units = 100 , 
                     input_shape = list (n_lags , 5)) %>%
  layer_dense ( units = 50, activation = "relu") %>% 
  layer_dense ( units = 1, activation = "sigmoid")
model %>% compile ( optimizer = optimizer_rmsprop() ,
                    loss = "binary_crossentropy",
                    metrics = c("accuracy") )
history <- model %>% 
  fit(xrnn[ istrain , , ] , arframe[ istrain , "open" ] ,
      batch_size = 64 , epochs = epochs ,
      validation_data = list ( xrnn [! istrain , , ] , arframe [! istrain , "open" ])
  )
kpred <- predict ( model , xrnn [! istrain , , ])
y_true <- factor(matrix(ydata[!istrain]))
kpred <- factor(round(kpred,0))
confusionMatrix(kpred, y_true)

