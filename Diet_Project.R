library(tidyverse)

setwd("~/Downloads/wisconsin_diet_project_data")
df <- read_csv("Wisconsin_diet_quant_T_wMeta_noSerum.csv")

#Maximum number of samples in selected diet type that can be higher than minimum germ free 
#(1 = 0 samples are higher than minimum germ free, 2 = 1 sample can be higher than minimum germ free...)
#Higher value increases number of features
a <- 1

#Minimum threshold for germ free samples 
#(1=minimum sample intensity, 2=second from minimum sample intensity...)
#Higher value increases number of features
b <- 1

#Number of times higher the minimum germ free sample has to be above the maximum of any other sample
c <- 100

featuresTable <- data.frame(features=0)
z <- 1

subset <- df[,1:5]

#loop for each diet type individually
for(diet in c("cellulose","whole_grains","legumes","vegetable")){

#rows with only germ free samples in selected diet type
gf_diet <- df[which((df$ATTRIBUTE_donor=="Germ_free") & (df$ATTRIBUTE_diet==diet)),]

  #select each sample donor individually
  for(donor in c("Conventional","WLS_165","WLS_291","WLS_147")){

  donor_diet <- df[which((df$ATTRIBUTE_donor==donor) & (df$ATTRIBUTE_diet==diet)),]

    for(i in 6:ncol(df)){
  
    gf_i <- gf_diet[[i]]
    #gf_i <- gf_i[which(gf_i!=0)] #remove zeros
    #if(length(gf_i)==0) {gf_i <- 0}
    gf_i <- sort(gf_i,decreasing=F)
    
    donor_i <- donor_diet[[i]]
    #donor_i <- donor_i[which(donor_i!=0)] #remove zeros
    #if(length(donor_i)==0) {donor_i <- 0}
    donor_i <- sort(donor_i,decreasing=T)
      
      if(gf_i[b] > (donor_i[a]*c)){
        if(!(colnames(df[,i]) %in% featuresTable[[1]])){#if the feature isn't already in the table
          featuresTable[z,1] <- colnames(df[,i])
          subset <- cbind(subset,df[colnames(df[,i])])
          z <- z+1
          }
      }
  
    }
    
  }

}

write.csv(featuresTable,"features.csv",row.names = F)
write.csv(subset,"subset.csv",row.names = F)

