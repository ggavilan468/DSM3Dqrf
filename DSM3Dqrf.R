#The data includes information on silt, sand, clay, and organic matter (OM) collected from soil pits, 
#with details on their respective depths (multiple samples were taken at different depths per soil pit)

library(quantregForest)
library(caret)
library(doParallel)

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

