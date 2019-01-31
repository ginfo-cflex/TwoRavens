## 
## rookmkdocs.R
## Build a folder structure and metadata to resemble MIT-LincolnLabs seed datasets
##


library(jsonlite)
library(foreign)


send <- function(res) {
    res <- jsonlite:::toJSON(res)
    if (production) {
        sink()
    }
    write(res, "../assets/result.json")

    response <- Response$new(headers = list("Access-Control-Allow-Origin" = "*"))
    response$write(res)
    response$finish()
}


#  to check if the variable is binary
is_binary <- function(v) {
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L
}

# seed_zombie function
# returns a list with datasetDoc and problemDoc
seed_zombie <- function(data, name, fraction=0.2, ID=NULL, citation="", description="", depvarname=NULL, taskType=NULL, taskSubType="", metric=NULL, seed=123 ){

  set.seed=seed

  if (is.null(ID)){
    ID=name
  }
  
  # check for d3mIndex, if none, add one
  if("d3mIndex" %in% colnames(data)) {
    # moving it to the first column, in case it's somewhere else
    data <- data[,c(which(colnames(data)=="d3mIndex"),which(colnames(data)!="d3mIndex"))]
  } else { # d3mIndex is not in column names, so adding it
    d3mIndex <- 1:nrow(data)
    data <- cbind(d3mIndex, data)
  }

  n <- nrow(data)
  ## Write datasetDoc json string

  columnlist = list()
  allNames <- names(data)
  locatedDV <- FALSE

  for(i in 1:ncol(data)){
 #   print(i)
 #   print(colnames(data)[i])
    tempdata <- data[,i]

    if(allNames[i] == depvarname){
      temprole <- "suggestedTarget"
      depvarColIndex <- i
      locatedDV <- TRUE
    }else if (i==1){
      temprole <- "index"
    }else {
      temprole <- "attribute"
    }
    
    # categorical (if non-numeric), real, or integer
    if(!is.numeric(tempdata)){
      temptype <- "categorical"
    } else if(any(!(round(tempdata)==tempdata), na.rm=TRUE)){
      temptype <- "real"
    } else {
      temptype <- "integer"
    }
    
    temp <- list(colIndex=i-1, colName=allNames[i], colType=temptype, role=I(temprole))
    columnlist[[i]]<- temp
  }


  if(!locatedDV){
    print("No variable name in dataset matched supplied `depvarname` argument.")
  }

  dataResourcesList <- list(resID="0", resPath="tables/learningData.csv", resType="table", resFormat=I("text/csv"), isCollection=FALSE, columns=columnlist)

  datasetID <- paste(ID,"_dataset",sep="")

  datasetdoclist <- list(about=list(datasetID=name, datasetName=ID, description=description, citation=citation, datasetSchemaVersion= "3.0"), dataResources=list(dataResourcesList))
  datasetDoc <- toJSON(datasetdoclist, auto_unbox=TRUE, pretty=TRUE)


  ## Write problemDoc json string
  
  problemdoclist <- list(about=list(problemID=paste(name,"_problem",sep=""), problemName=ID, problemDescription=description, taskType=taskType, taskSubType=taskSubType, problemVersion="1.0", problemSchemaVersion="3.0"),
    inputs=list(data=list(list(datasetID=datasetID, targets=list(list(colIndex=depvarColIndex, colName=depvarname)) )),
    dataSplits=list(method="holdOut", testSize=fraction), performanceMetrics=list(list(metric=metric))),
    expectedOutputs=list(predictionsFile="predictions.csv"))

    # removing taskSubType if "remove"
  if(problemdoclist$taskSubType=="remove") problemdoclist$taskSubType <- NULL

  problemDoc <- toJSON(problemdoclist, auto_unbox=TRUE, pretty=TRUE)
  
  out <- list(problemDoc=problemDoc, datasetDoc=datasetDoc)
  return(out)
}


mkdocs.app <- function(env) {
    print(env)
    production <- FALSE
    result <- list()

    if (production) {
        sink(file = stderr(), type = "output")
    }

    request <- Request$new(env)
    valid <- jsonlite::validate(request$POST()$solaJSON)
    if (! valid) {
        return(send(list(warning = "The request is not valid json. Check for special characters.")))
    }

    everything <- jsonlite::fromJSON(request$POST()$solaJSON, flatten = TRUE)
    print("everything: ")
    print(everything)

    dataurl <- everything$datafile
    if (is.null(dataurl)) {
        return(send(list(warning = "No data url."))) # required
    }
    
    dataid <- everything$datasetid
    if (is.null(dataid)) {
        return(send(list(warning = "No dataset id."))) # required
    }
    
    dataname <- everything$name
    if (is.null(dataname)) {
        return(send(list(warning = "No dataset name."))) # required
    }

    datadesc <- everything$description
    if (is.null(datadesc)) {
        datadesc <- "No dataset description." # default
#        return(send(list(warning = "No dataset description.")))
    }
    
    tasktype <- everything$taskType
    if (is.null(tasktype)) {
        return(send(list(warning = "No task type."))) # required
    }
    
    tasksubtype <- everything$taskSubType
    if (is.null(tasksubtype)) {
        tasksubtype <- "remove" # optional, and will remove when writing problem doc
    }
    
#depvarname: data.targets[...] from problem doc, can be more than one object
#metric: performanceMetrics[...] from problem doc, can be more than one

    separator <- if (endsWith(dataurl, 'csv'))',' else '\t'
    mydata <- read.table(dataurl, sep = separator, header = TRUE, fileEncoding = 'UTF-8')

    tryCatch({
        docs <- seed_zombie(data=mydata, name=dataname, depvarname="war", ID=dataid, description=description, taskType=tasktype, taskSubType=tasksubtype, metric="f1Macro")
      },
    error = function(err) {
        result <<- list(warning = paste("error: ", err))
        print("result ---- ")
        print(result)
    })

    return(send(result))
}
