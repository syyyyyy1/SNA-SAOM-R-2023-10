library(RSiena)
#将数据转换为Siena可以识别的对象
friends <- sienaNet(array(c(adjt1,adjt2), dim=c(202,202,2)))
CB <- sienaDependent(cbmatrix, type="behavior")
CBV <- sienaDependent(cbvmatrix, type="behavior")

#Sienadata
sienaData <- sienaDataCreate(friends,CB)
sienaData02 <- sienaDataCreate(friends,CBV)

#设置模型
groupEffects02 <- getEffects(sienaData02)
groupEffects02 <- includeEffects(groupEffects02,density,recip,between,test=F,fix=F)
groupEffects02 <- includeEffects(groupEffects02,sameX,interaction1="CBV",
                               test=F,fix=F)
groupEffects02 <- includeEffects(groupEffects02,name = "CBV", avSim,interaction1="friends",
                               test=F,fix=F)
groupModel02 <- sienaModelCreate(useStdInits = FALSE,projname='cbullying1015', seed = 919113)
results02 <- siena07(data = sienaData02,effects = groupEffects02, groupModel,batch = F, verbose = F,
                   useCluster = T, nbrNodes = 4,initC = T,returnDeps = T)
parameter <-results02$effects$effectName
estimate <- results02$theta
st.error <- sqrt(diag(results02$covtheta))
normal.variate <- estimate/st.error
p.value.2sided <- 2*pnorm(abs(normal.variate),lower.tail=FALSE)
data.frame(parameter,
           estimate=round(estimate,3),
           st.error=round(st.error,3),
           normal.variate=round(normal.variate,2),
           p.value=round(p.value.2sided,4)
)

#网络结构效应
groupEffects01 <- getEffects(sienaData)
groupEffects01 <- includeEffects(groupEffects01,density,recip,test=F,fix=F)
#选择效应
groupEffects01 <- includeEffects(groupEffects01,simX,interaction1="CB",
                               test=F,fix=F)
groupEffects01 <- includeEffects(groupEffects01,isolate,interaction1="friends",name="CB",
                                 test=F,fix=F)
#影响效应
groupEffects01 <- includeEffects(groupEffects01,name = "CB", avSim,interaction1="friends",
                               test=F,fix=F)

groupModel <- sienaModelCreate(useStdInits = FALSE,projname='cbullying1015', seed = 919113)
results <- siena07(data = sienaData,effects = groupEffects01, groupModel,batch = F, verbose = F,
                          useCluster = T, nbrNodes = 4,initC = T,returnDeps = T)
#绘制P值
parameter <-results$effects$effectName
estimate <- results$theta
st.error <- sqrt(diag(results$covtheta))
normal.variate <- estimate/st.error
p.value.2sided <- 2*pnorm(abs(normal.variate),lower.tail=FALSE)
data.frame(parameter,
           estimate=round(estimate,3),
           st.error=round(st.error,3),
           normal.variate=round(normal.variate,2),
           p.value=round(p.value.2sided,4)
)

#加入控制变量
agecovar <- coCovar(agematrix[,1])
gendercovar <- coCovar( gendermatrix[ , 1 ] )
pressurecovar <- coCovar( presssurematrix[ , 1 ] )
peer_attachcovar <- coCovar( peer_attachmatrix[ , 1 ] )
socialnormcovar <- coCovar( socialnormmatrix[ , 1 ] )
psyhealthcovar <- coCovar( psyhealthmatrix[ , 1 ] )
socialvlasscovar <- coCovar( adjsclass[ , 1 ] )

victim <- coCovar( cbvmatrix[,1] )
paMediation <- coCovar( ParentMeMatrix[,1] )
sfcontrol <- coCovar( selfcontrolmatrix[,1] )
mediau <- varCovar( mediamatrix )

myEffects <- setEffect(myEffects, transRecTrip,
                       include=T, fix=T, test=T)


MysienaData <- sienaDataCreate(friends,CB,victim,paMediation,sfcontrol,agecovar,gendercovar,pressurecovar,socialvlasscovar)

#设置模型
MygroupEffects <- getEffects(MysienaData)
# effFrom,interaction1=paMediation, agecovar, gendercovar,pressurecovar,victim, sfcontrol,socialvlasscovar
# altX, interaction1=CB,victim,agecovar, gendercovar,socialvlasscovar
# egoX, interaction1=Cb,victim,agecovar,gendercovar,socialvlasscovar

#网络动态
MygroupEffects <- includeEffects(MygroupEffects,density,recip,test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,egoX,interaction1="CB",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,egoX,interaction1="victim",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,egoX,interaction1="agecovar",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,egoX,interaction1="gendercovar",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,altX,interaction1="CB",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,altX,interaction1="victim",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,altX,interaction1="agecovar",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,altX,interaction1="gendercovar",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="firends",diffX,interaction1="CB",
                                 test=F,fix=F)
#行为动态
# effFrom,interaction1=paMediation, agecovar, gendercovar,pressurecovar,victim, sfcontrol,socialvlasscovar
MygroupEffects <- includeEffects(MygroupEffects,name="CB", avSim,interaction1 = "friends",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="CB", effFrom,interaction1="victim",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="CB", effFrom,interaction1="paMediation",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="CB", effFrom,interaction1="pressurecovar",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="CB", effFrom,interaction1="sfcontrol",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="CB", effFrom,interaction1="agecovar",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="CB", effFrom,interaction1="gendercovar",
                                 test=F,fix=F)
MygroupEffects <- includeEffects(MygroupEffects,name="CB", effFrom,interaction1="socialvlasscovar",
                                 test=F,fix=F)

MygroupModel <- sienaModelCreate(useStdInits = FALSE,projname='cbullying1018', seed = 919113)
Myresults <- siena07(data = MysienaData,effects = MygroupEffects, MygroupModel,batch = F, verbose = F,
                     useCluster = T, nbrNodes = 5,initC = T,returnDeps = T)
#绘制P值
parameter <-Myresults$effects$effectName
estimate <- Myresults$theta
st.error <- sqrt(diag(Myresults$covtheta))
normal.variate <- estimate/st.error
p.value.2sided <- 2*pnorm(abs(normal.variate),lower.tail=FALSE)
data.frame(parameter,
           estimate=round(estimate,3),
           st.error=round(st.error,3),
           normal.variate=round(normal.variate,2),
           p.value=round(p.value.2sided,4)
)







