loadSimpData <- function(){
datMat <- matrix(data=c(1,2,1.3,1,2,2.1,1.1,1,1,1),nrow=5,ncol=2)
classLables <- matrix(data=c(1,1,-1,-1,1),nrow=1,ncol=5)
return(list(data1=datMat,data2=classLables))#注意R中返回多个值时的方法
}
datMat=loadSimpData()[[1]]#注意R中调用list中具体元素的方法，两个[]
classLables=loadSimpData()[[2]]

stumpClassify <- function(dataMatrix,dimen,threshVal,threshIneq){
  dimen=as.numeric(dimen)  #加上这3个数据转化，是为了蛋疼的R数据结构
  threshVal=as.numeric(threshVal)
  threshIneq=as.character(threshIneq)
  retArray <- matrix(data=1,nrow=dim(dataMatrix)[1],ncol=1)#开始假设所有的都分对了
  if (threshIneq == 'lt'){
    retArray[dataMatrix[,dimen] <= threshVal] <- -1 #retArray[这里面的返回值是F和T]，如果是小于为假，那么小于阈值的为假
  }else{
    retArray[dataMatrix[,dimen] > threshVal] <- -1 #如果是大于，那么大于阈值为假
  }
  return(retArray)#返回此次阈值测试的结果
}

buildStump <- function(dataArr,classLabels,D){
  dataMatrix=as.matrix(dataArr);labelMat=t(as.matrix(classLables))
  m=dim(dataMatrix)[1] #行数
  n=dim(dataMatrix)[2] #列数
  numSteps=10.0 #步数
  bestStump=list() #Python中使用的是字典，我们这里使用list
  bestClasEst=matrix(data=0,nrow=m,ncol=1) #最好的分类结果
  minError=Inf #inf是不行的，Python里面可以，但是R里面只有Inf才可以，注意大小写
  for (i in seq(1,n)){#按照有变量个数进行测试，这里数据只有两个
    rangMin=min(dataMatrix[,i]);rangMax=max(dataMatrix[,i]) #测试阈值的最大与最小
    stepSize=(rangMax-rangMin)/numSteps
    for (j in seq(-1,as.integer(numSteps)+1)){#这里的步长选择了-1到11，阈值在最大最小之外，是可以的。
      for (inequal in c("lt","gt")){#这里面的lt表示小于，gt表示大于，大于小于的意思就是决策树里面的左拐还是右拐
        threshVal=rangMin + j*stepSize
        predictedVals <- stumpClassify(dataMatrix,i,threshVal,inequal)
        errArr <- matrix(data=1,nrow=m,ncol=1)
        errArr[predictedVals == labelMat]=0
        weightedArr <- sum(t(D)*errArr)#这个地方和Python中不一样，Python中直接求和了，R中没有，需要加一个
        if (weightedArr < minError){
          minError = weightedArr
          bestClasEst = predictedVals #原来Python中用的copy函数，这是因为Python中拷贝的问题，R中没有这个问题
          bestStump$dim = i
          bestStump$thresh = threshVal
          bestStump$ineq = inequal
        }
      }
    }
  }
  return(list(bestStump,minError,bestClasEst))
}

sampleNum = dim(datMat)[1]
D=matrix(data=1/sampleNum,nrow=1,ncol=sampleNum) #这个和原文有点出入
result=buildStump(datMat,classLables,D)


adaBoostTrainDS <- function(dataArr,classLables,numIt=40){
  dataArr=as.matrix(dataArr)
  classLables=as.matrix(classLables)
  weakClassArr=list() #Python中用字典，我们依然用list
  m=dim(dataArr)[1]
  D=matrix(data=1/m,nrow=1,ncol=5)
  aggClassEst=matrix(data=0,nrow=m,ncol=1)
  for (i in seq(1,numIt)){
    result=buildStump(datMat,classLables,D)#这个就是R里面比Python笨的地方
    bestStump=result[[1]]
    error=result[[2]]
    classEst=result[[3]]
    alpha=0.5*log((1.0-error)/max(error,1e-16))
    bestStump$alpha=alpha#在这里面R与Python的不同，觉得要简单点，Python的[]用的是append 
    expon=-1*alpha*classLables*t(classEst)#这个地方是R简单点的地方，R中要求两个相乘的vector方向一致，multipy是一对一的相乘
    D=D*exp(expon)#这个地方到底是谁乘以谁有点烦躁
    D=D/sum(D)
    aggClassEst = aggClassEst + alpha*classEst
    aggErrors = matrix(data=1,nrow=1,ncol=5)
    aggErrors[sign(aggClassEst) == t(classLables)] = 0
    errorRate=sum(aggErrors)/m
    #result$errorRate=errorRate
    weakClassArr[[paste0(i,"thclassfier")]]=bestStump#蛋疼的R数据结构啊啊啊啊
    if (errorRate <= 0){
      break
    }
  }
  return(weakClassArr)
}

result2=adaBoostTrainDS(datMat,classLables)

adaClassify <- function(datToClass,classifierArr){
  dataMatrix=datToClass
  m=dim(dataMatrix)[1]
  aggClassEst=matrix(data=0,nrow=m,ncol=1)
  for (i in seq(length(classifierArr))){
    classEst=stumpClassify(dataMatrix,classifierArr[[i]]["dim"],classifierArr[[i]]["thresh"],classifierArr[[i]]["ineq"])
    aggClassEst=aggClassEst+as.numeric(classifierArr[[i]]["alpha"])*classEst
  }
  return(sign(aggClassEst))
}

datToClass=matrix(data=c(5,0,5,0),nrow=2,ncol=2) #蛋疼的数据结构，注意一行是一个数据
result3=adaClassify(datToClass,result2)

library(adabag)
model.bg = boosting(Species ~ ., data = iris)
pre.bg = predict(model.bg, iris)
