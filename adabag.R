loadSimpData <- function(){
datMat <- matrix(data=c(1,2,1.3,1,2,2.1,1.1,1,1,1),nrow=5,ncol=2)
classLables <- matrix(data=c(1,1,-1,-1,1),nrow=1,ncol=5)
return(list(data1=datMat,data2=classLables))#ע��R�з��ض��ֵʱ�ķ���
}
datMat=loadSimpData()[[1]]#ע��R�е���list�о���Ԫ�صķ���������[]
classLables=loadSimpData()[[2]]

stumpClassify <- function(dataMatrix,dimen,threshVal,threshIneq){
  dimen=as.numeric(dimen)  #������3������ת������Ϊ�˵��۵�R���ݽṹ
  threshVal=as.numeric(threshVal)
  threshIneq=as.character(threshIneq)
  retArray <- matrix(data=1,nrow=dim(dataMatrix)[1],ncol=1)#��ʼ�������еĶ��ֶ���
  if (threshIneq == 'lt'){
    retArray[dataMatrix[,dimen] <= threshVal] <- -1 #retArray[������ķ���ֵ��F��T]�������С��Ϊ�٣���ôС����ֵ��Ϊ��
  }else{
    retArray[dataMatrix[,dimen] > threshVal] <- -1 #����Ǵ��ڣ���ô������ֵΪ��
  }
  return(retArray)#���ش˴���ֵ���ԵĽ��
}

buildStump <- function(dataArr,classLabels,D){
  dataMatrix=as.matrix(dataArr);labelMat=t(as.matrix(classLables))
  m=dim(dataMatrix)[1] #����
  n=dim(dataMatrix)[2] #����
  numSteps=10.0 #����
  bestStump=list() #Python��ʹ�õ����ֵ䣬��������ʹ��list
  bestClasEst=matrix(data=0,nrow=m,ncol=1) #��õķ�����
  minError=Inf #inf�ǲ��еģ�Python������ԣ�����R����ֻ��Inf�ſ��ԣ�ע���Сд
  for (i in seq(1,n)){#�����б����������в��ԣ���������ֻ������
    rangMin=min(dataMatrix[,i]);rangMax=max(dataMatrix[,i]) #������ֵ���������С
    stepSize=(rangMax-rangMin)/numSteps
    for (j in seq(-1,as.integer(numSteps)+1)){#����Ĳ���ѡ����-1��11����ֵ�������С֮�⣬�ǿ��Եġ�
      for (inequal in c("lt","gt")){#�������lt��ʾС�ڣ�gt��ʾ���ڣ�����С�ڵ���˼���Ǿ������������ջ����ҹ�
        threshVal=rangMin + j*stepSize
        predictedVals <- stumpClassify(dataMatrix,i,threshVal,inequal)
        errArr <- matrix(data=1,nrow=m,ncol=1)
        errArr[predictedVals == labelMat]=0
        weightedArr <- sum(t(D)*errArr)#����ط���Python�в�һ����Python��ֱ������ˣ�R��û�У���Ҫ��һ��
        if (weightedArr < minError){
          minError = weightedArr
          bestClasEst = predictedVals #ԭ��Python���õ�copy������������ΪPython�п��������⣬R��û���������
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
D=matrix(data=1/sampleNum,nrow=1,ncol=sampleNum) #�����ԭ���е����
result=buildStump(datMat,classLables,D)


adaBoostTrainDS <- function(dataArr,classLables,numIt=40){
  dataArr=as.matrix(dataArr)
  classLables=as.matrix(classLables)
  weakClassArr=list() #Python�����ֵ䣬������Ȼ��list
  m=dim(dataArr)[1]
  D=matrix(data=1/m,nrow=1,ncol=5)
  aggClassEst=matrix(data=0,nrow=m,ncol=1)
  for (i in seq(1,numIt)){
    result=buildStump(datMat,classLables,D)#�������R�����Python���ĵط�
    bestStump=result[[1]]
    error=result[[2]]
    classEst=result[[3]]
    alpha=0.5*log((1.0-error)/max(error,1e-16))
    bestStump$alpha=alpha#��������R��Python�Ĳ�ͬ������Ҫ�򵥵㣬Python��[]�õ���append 
    expon=-1*alpha*classLables*t(classEst)#����ط���R�򵥵�ĵط���R��Ҫ��������˵�vector����һ�£�multipy��һ��һ�����
    D=D*exp(expon)#����ط�������˭����˭�е㷳��
    D=D/sum(D)
    aggClassEst = aggClassEst + alpha*classEst
    aggErrors = matrix(data=1,nrow=1,ncol=5)
    aggErrors[sign(aggClassEst) == t(classLables)] = 0
    errorRate=sum(aggErrors)/m
    #result$errorRate=errorRate
    weakClassArr[[paste0(i,"thclassfier")]]=bestStump#���۵�R���ݽṹ��������
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

datToClass=matrix(data=c(5,0,5,0),nrow=2,ncol=2) #���۵����ݽṹ��ע��һ����һ������
result3=adaClassify(datToClass,result2)

library(adabag)
model.bg = boosting(Species ~ ., data = iris)
pre.bg = predict(model.bg, iris)