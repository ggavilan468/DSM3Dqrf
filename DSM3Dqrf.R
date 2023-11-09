#The data in "dat" includes information on silt, sand, clay, and soil organic matter (SOM) collected from soil pits, 
#with details on their respective depths (multiple samples were taken at different depths per soil pit), and the SCORPAN variables

library(quantregForest)
library(caret)
library(doParallel)

#example for SOM
# Sd and percentile predictions

# Search for the best mtry parameter
rfmodel <- train(fm2, data = dat, method = "qrf", trControl = controlmtry, importance = TRUE)

SOMmodel <- quantregForest(x=dat[,c("DEPTH_g","biovars.1","biovars.12","biovars.2","Channel_Network_Base_Level",
                                    "biovars.4","dem","Slope","Diffuse_Insolation","MRVBF")],y=dat$mo, nthreads=25, keep.inbag=FALSE, mtry=3)

#percentile prediction
predict(object = SOMmodel, newdata=framecovs, what=c(0.1,0.25,0.5,0.75,0.9))
#standard deviation prediction
predict(object = SOMmodel, newdata=framecovs, what=sd)

#Example for 3d SOM representation
fm2 = as.formula(paste0("MO ~ DEPTH +",  paste(topvariables,
                                                  collapse = "+")))
# Search for the best mtry parameter
rfmodel <- train(fm2, data = dat, method = "qrf", trControl = controlmtry, importance = TRUE)

MO_model <- quantregForest(x=dat[,c("DEPTH_g","biovars.1","biovars.12","biovars.2","Channel_Network_Base_Level",
                                    "biovars.4","dem","Slope","Diffuse_Insolation","MRVBF")],y=dat$mo, nthreads=25, keep.inbag=FALSE, mtry=3)

#how to get MO prediction at different depths
beginCluster()
for(i in seq(0,180,10)){
  print(paste("layer", i, "cm"))
  values(covs$DEPTH_g) = i
  pred <- clusterR(covs, predict, args=list(model=MO_model))
  nm1 = paste0("MO_", i, "cm")
  names(pred) <- nm1
  resultado <- stack(resultado, pred)
  gc()
}
endCluster()

