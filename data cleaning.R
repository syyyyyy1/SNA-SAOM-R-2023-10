library(dplyr)
library(tidyr)
#加入班级变量
dfall <- entire_sample%>%
  mutate(class = as.numeric(substr(egoid, 1, 1)))

#对于CBV 和CB进行重新赋值
#     小于等于1为0，代表无，大于1为1，代表有
#     运用ifelse()函数进行重新赋值
dfall$cbvictim <- ifelse(dfall$cbvictim > 1, 1, 0)
dfall$cbullying <- ifelse(dfall$cbullying > 1, 1, 0)

#提取两个时段的数据
t1df <- dfall %>%
  filter(point == "t1")
t2df <- dfall %>%
  filter(point == "t2")
#年龄变量加入t1df


#提取两个时段的网络
##T1时网络
###转换为edgelist
###把数据变成from_id → to_id的格式(edgelist)
###提名对象(to_id)可能有NA，过滤NA值，过滤只被提名未提名他人的观察对象
###但是需要清除重复值
t1edge <- t1df %>%
  select(class, egoid, starts_with("frd")) %>%
  gather(key = "varname", value = "to", -class, -egoid)%>%
  filter(!is.na(to),to %in% egoid) %>%
  mutate(time     = "t1",
         same_class = class == as.numeric(substr(to, 1, 1)))
t1edge#生成了1693条边

t1edge <- t1edge %>%
  distinct(egoid, to, .keep_all = TRUE)#去除重复值后，该网络内是803条边

##T2时网络
t2edge <- t2df %>%
  select(class, egoid, starts_with("frd")) %>%
  gather(key = "varname", value = "to", -class, -egoid)%>%
  filter(!is.na(to),to %in% egoid) %>%
  mutate(time     = "t2",
         same_class = class == as.numeric(substr(to, 1, 1)))
t2edge#生成了1397条边

t2edge <- t2edge %>%
  distinct(egoid, to, .keep_all = TRUE)#去除重复值后，该网络内是626条边

#固定排序并且转换为数字
all_p<- intersect(t1edge$egoid, t2edge$egoid)#求交集，识别饱和样本
t1df <- t1df %>%
  filter(egoid %in% all_p) 
t2df <- t2df %>%
  filter(egoid %in% all_p) 
t1edge <- filter(t1edge, egoid %in% all_p) 
t2edge <- filter(t2edge, egoid %in% all_p) 
t1df <- mutate(t1df, eorder=factor(egoid, order=T,levels=all_p))#重新固定egoid顺序
t1df$eorder <- as.numeric(t1df$eorder)#将顺序转换为数字对象
t2df <- mutate(t2df, eorder=as.numeric(factor(egoid, levels=all_p, order=T)))

#邻接矩阵
##t1网络 799条边
t1edge <- mutate(t1edge, e_order=as.numeric(factor(egoid, levels=all_p, order=T)))
t1edge <- mutate(t1edge, to_order=as.numeric(factor(to, levels=all_p, order=T)))
t1edge <- mutate(t1edge, tie=1)
t1frd <- t1edge[,7:9]
t1frd <- filter(t1frd, is.na(to_order)==FALSE)
t1frd <- as.matrix(t1frd)
class(t1frd)

adjt1 <- matrix(0, length(all_p),length(all_p))
adjt1[t1frd[,1:2]] <- t1frd[,3]
write.table(adjt1, file = 'adjt1.txt' , sep= ',', row.names = FALSE, col.names = FALSE)


##t2网络 621条边
t2edge <- mutate(t2edge, e_order=as.numeric(factor(egoid, levels=all_p, order=T)))
t2edge <- mutate(t2edge, to_order=as.numeric(factor(to, levels=all_p, order=T)))
t2edge <- mutate(t2edge, tie=1)
t2frd <- t2edge[,7:9]
t2frd <- filter(t2frd, is.na(to_order)==FALSE)
t2frd <- as.matrix(t2frd)
class(t2frd)

adjt2 <- matrix(0, length(all_p),length(all_p))
adjt2[t2frd[,1:2]] <- t2frd[,3]
write.table(adjt2, file = 'adjt2.txt' , sep= ',', row.names = FALSE, col.names = FALSE)


#所有变量转换为矩阵形式
## 社会阶层
socialclass <- socialclass %>%
  filter(egoid %in% all_p)%>%
  mutate(socialclass, p_order=as.numeric(factor(egoid, levels=all_p,order=T)))
adjvar <- matrix(0, length(all_p), ncol = 2)

adjsocialclass <- socialclass[,2:3]
adjsocialclass<- filter(adjsocialclass, is.na(p_order)==FALSE)
adjsocialclass <- as.matrix(adjsocialclass)
adjsclass <- adjvar
adjsclass[adjsocialclass[,2],] <- adjsocialclass[,1]
adjsclass <- adjsclass [,2]
write.table(adjsclass, file = 'socialclass.txt' , sep= '', row.names = FALSE, col.names = FALSE)


cbullying <-merge(t1df[,c(4,50)], t2df[, c(4, 50)], by = "eorder")
cbullying <- as.matrix(cbullying)
cbmatrix <- adjvar
cbmatrix[cbullying[,1],] <- cbullying[,2:3]
write.table(cbmatrix, file = 'cbullying.txt' , sep= '', row.names = FALSE, col.names = FALSE)

cbvictim <-merge(t1df[,c(3,50)], t2df[, c(3, 50)], by = "eorder")
cbvictim <- as.matrix(cbvictim)
cbvmatrix <- adjvar
cbvmatrix[cbvictim [,1],] <- cbvictim [,2:3]
write.table(cbvmatrix, file = 'cbvictim.txt' , sep= '', row.names = FALSE, col.names = FALSE)



age <-t1df[,c(29,50)]
age <- as.matrix(age) 
agematrix <- adjvar
agematrix [age[,2],] <- age[,1]
agematrix <- agematrix[,1]
write.table(agematrix, file = 'age.txt' , sep= '', row.names = FALSE, col.names = FALSE)


gender <-t1df[,c(30,50)]
gender <- as.matrix(gender) 
gendermatrix <- adjvar
gendermatrix [gender[,2],] <- gender[,1]
gendermatrix <- gendermatrix[,1]
write.table(gendermatrix, file = 'gender.txt' , sep= '', row.names = FALSE, col.names = FALSE)


origin <-t1df[,c(31,50)]
origin <- as.matrix(origin)  
originmatrix <- adjvar
originmatrix [origin[,2],] <- origin[,1]
originmatrix <- originmatrix[,1]
write.table(originmatrix, file = 'origin.txt' , sep= '', row.names = FALSE, col.names = FALSE)

onlychild <-t1df[,c(32,50)]
onlychild <- as.matrix(onlychild)  
onlychildmatrix <- adjvar
onlychildmatrix [onlychild[,2],] <- onlychild[,1]
onlychildmatrix <- onlychildmatrix[,1]
write.table(onlychildmatrix, file = 'onlychild.txt' , sep= '', row.names = FALSE, col.names = FALSE)

socialnorm <-t1df[,c(35,50)]
socialnorm <- as.matrix(socialnorm)  
socialnormmatrix <- adjvar
socialnormmatrix [socialnorm[,2],] <- socialnorm[,1]
socialnormmatrix <- socialnormmatrix[,1]
write.table(socialnormmatrix, file = 'socialnorm.txt' , sep= '', row.names = FALSE, col.names = FALSE)

social_support <-t1df[,c(38,50)]
social_support  <- as.matrix(social_support)  
social_supportmatrix <- adjvar
social_supportmatrix [social_support[,2],] <- social_support[,1]
social_supportmatrix <- social_supportmatrix[,1]
write.table(social_supportmatrix, file = 'social_support.txt' , sep= '', row.names = FALSE, col.names = FALSE)

selfesteem <-t1df[,c(39,50)]
selfesteem <- as.matrix(selfesteem)  
selfesteemmatrix <- adjvar
selfesteemmatrix [selfesteem[,2],] <- selfesteem[,1]
selfesteemmatrix <- selfesteemmatrix[,1]
write.table(selfesteemmatrix, file = 'selfesteem.txt' , sep= '', row.names = FALSE, col.names = FALSE)

psyhealth <-t2df[,c(46,50)]
psyhealth <- as.matrix(psyhealth)  
psyhealthmatrix <- adjvar
psyhealthmatrix [psyhealth[,2],] <- psyhealth[,1]
psyhealthmatrix <- psyhealthmatrix[,1]
write.table(psyhealthmatrix, file = 'psyhealth.txt' , sep= '', row.names = FALSE, col.names = FALSE)

presssure <-t2df[,c(47,50)]
presssure <- as.matrix(presssure)  
presssurematrix <- adjvar
presssurematrix [presssure[,2],] <- presssure[,1]
presssurematrix <- presssurematrix[,1]
write.table(presssurematrix, file = 'presssure.txt' , sep= '', row.names = FALSE, col.names = FALSE)

parent_attach <-t2df[,c(44,50)]
parent_attach <- as.matrix(parent_attach)  
parent_attachmatrix <- adjvar
parent_attachmatrix [parent_attach[,2],] <- parent_attach[,1]
parent_attachmatrix <- parent_attachmatrix[,1]
write.table(parent_attachmatrix, file = 'parent_attach.txt' , sep= '', row.names = FALSE, col.names = FALSE)

peer_attach <-t2df[,c(45,50)]
peer_attach <- as.matrix(peer_attach)  
peer_attachmatrix <- adjvar
peer_attachmatrix [peer_attach[,2],] <- peer_attach[,1]
peer_attachmatrix <- peer_attachmatrix[,1]
write.table(peer_attachmatrix, file = 'peer_attach.txt' , sep= '', row.names = FALSE, col.names = FALSE)

coping_support <-t2df[,c(48,50)]
coping_support <- as.matrix(coping_support)  
coping_supportmatrix <- adjvar
coping_supportmatrix [coping_support[,2],] <- coping_support[,1]
coping_supportmatrix <- coping_supportmatrix[,1]
write.table(coping_supportmatrix, file = 'coping_support.txt' , sep= '', row.names = FALSE, col.names = FALSE)

selfcontrol <-merge(t1df[,c(36,50)], t2df[, c(36, 50)], by = "eorder")
selfcontrol <- as.matrix(selfcontrol)
selfcontrolmatrix <- adjvar
selfcontrolmatrix [selfcontrol[,1],] <- selfcontrol[,2:3]
write.table(selfcontrolmatrix, file = 'selfcontrol.txt' , sep= '', row.names = FALSE, col.names = FALSE)

ParentMediation <-merge(t1df[,c(43,50)], t2df[, c(43, 50)], by = "eorder")
ParentMediation <- as.matrix(ParentMediation)
ParentMeMatrix <- adjvar
ParentMeMatrix[ParentMediation[,1],] <- ParentMediation[,2:3]
write.table(ParentMeMatrix, file = 'ParentMediation.txt' , sep= '', row.names = FALSE, col.names = FALSE)

mediause <-merge(t1df[,c(37,50)], t2df[, c(37, 50)], by = "eorder")
mediause <- as.matrix(mediause)
mediamatrix <- adjvar
mediamatrix[mediause [,1],] <- mediause [,2:3]
write.table(mediamatrix, file = 'mediause.txt' , sep= '', row.names = FALSE, col.names = FALSE)

liveinschool <-merge(t1df[,c(34,50)], t2df[, c(34, 50)], by = "eorder")
liveinschool <- as.matrix(liveinschool)
liveinschoolmatrix <- adjvar
liveinschoolmatrix[liveinschool[,1],] <- liveinschool[,2:3]
write.table(liveinschoolmatrix, file = 'liveinschool.txt' , sep= '', row.names = FALSE, col.names = FALSE)

studentleader <-merge(t1df[,c(33,50)], t2df[, c(33, 50)], by = "eorder")
studentleader <- as.matrix(studentleader)
studentleadermatrix <- adjvar
studentleadermatrix[studentleader[,1],] <- studentleader[,2:3]
write.table(studentleadermatrix, file = 'studentleader.txt' , sep= '', row.names = FALSE, col.names = FALSE)

write.table(adjsclass, file = 'socialclass.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(cbmatrix, file = 'cbullying.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(cbvmatrix, file = 'cbvictim.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(agematrix, file = 'age.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(gendermatrix, file = 'gender.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(originmatrix, file = 'origin.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(onlychildmatrix, file = 'onlychild.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(socialnormmatrix, file = 'socialnorm.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(social_supportmatrix, file = 'social_support.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(selfesteemmatrix, file = 'selfesteem.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(psyhealthmatrix, file = 'psyhealth.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(presssurematrix, file = 'presssure.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(parent_attachmatrix, file = 'parent_attach.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(peer_attachmatrix, file = 'peer_attach.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(coping_supportmatrix, file = 'coping_support.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(selfcontrolmatrix, file = 'selfcontrol.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(ParentMeMatrix, file = 'ParentMediation.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(mediamatrix, file = 'mediause.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(liveinschoolmatrix, file = 'liveinschool.txt' , sep= ',', row.names = FALSE, col.names = FALSE)
write.table(studentleadermatrix, file = 'studentleader.txt' , sep= ',', row.names = FALSE, col.names = FALSE)

summary(cbt1$cbullying)
summary(cbt2$cbullying)

cbt1 <- entire_sample%>%
  filter(point == "t1")
cbt2 <- entire_sample%>%
  filter(point == "t2")
