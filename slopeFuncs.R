###hakimabdi/MKtrend.R

MKraster <- function(rasterstack, type=c("trend","pval","both")){
  
  # Values for (layers, ncell, ncol, nrow, method, crs, extent) come straight from the input raster stack
  # e.g. nlayers(rasterstack), ncell(rasterstack)... etc.
  print(paste("Start MKraster:",Sys.time()))
  print("Loading parameters")
  layers=nlayers(rasterstack);ncell=ncell(rasterstack);
  ncol=ncol(rasterstack);nrow=nrow(rasterstack);crs=crs(rasterstack);
  extent=extent(rasterstack);pb = txtProgressBar(min = 0, max = ncell, initial = 0)
  print("Done loading parameters")
  mtrx <- as.matrix(rasterstack,ncol=layers)
  empt <- matrix(nrow=ncell, ncol=2)
  
  print("Initiating loop operation")
  if (type == "trend"){
    
    for (i in 1:length(mtrx[,1])){
      if (all(is.na(mtrx[i,]))){ 
        empt[i,1] <- NA 
      } else 
        if (sum(!is.na(mtrx[i,])) < 4){
          empt[i,1] <- NA 
        } else 
          empt[i,1] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$tau)
    }
    
    print("Creating empty raster")
    trend <- raster(nrows=nrow,ncols=ncol,crs=crs)
    extent(trend) <- extent
    print("Populating trend raster")
    values(trend) <- empt[,1]
    print(paste("Ending MKraster on",Sys.time()))
    trend
  } 
  else
    if (type == "pval"){
      
      for (i in 1:length(mtrx[,1])){
        if (all(is.na(mtrx[i,]))){ 
          empt[i,1] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,])) < 4){
            empt[i,1] <- NA 
          } else 
            empt[i,1] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$sl)
      }
      
      pval <- raster(nrows=nrow,ncols=ncol,crs=crs)
      extent(pval) <- extent
      print("Populating significance raster")
      values(pval) <- empt[,1]
      print(paste("Ending MKraster on",Sys.time()))
      pval
    }
  else
    if (type == "both"){
      
      for (i in 1:length(mtrx[,1])){
        if (all(is.na(mtrx[i,]))){ 
          empt[i,1] <- NA 
        } else 
          if (sum(!is.na(mtrx[i,])) < 4){
            empt[i,1] <- NA 
          } else 
            tryCatch({
              empt[i,1] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$tau)
              empt[i,2] <- as.numeric(MannKendall(as.numeric(mtrx[i,]))$sl)
            })
      }
      
      tr <- raster(nrows=nrow,ncols=ncol,crs=crs)
      pv <- raster(nrows=nrow,ncols=ncol,crs=crs)
      print("Populating raster brick")
      values(tr) <- empt[,1]
      values(pv) <- empt[,2]
      brk <- brick(tr,pv)
      extent(brk) <- extent
      names(brk) <- c("trend","p.value")
      print(paste("Ending MKraster on",Sys.time()))
      brk
    }
}

###stackoverflow/Loïc Dutrieux
fun2 <- function(x) {
  model <- summary(lm(x ~ time)); 
  c(model$coefficients[2,1], model$r.squared) 
}

###stackoverflow/
trend.func <- function(x) { 
  if (is.na(x[1])){ NA } else {m <- lm(x ~ yearNames);
  summary(m)$coefficients[2] }
}


