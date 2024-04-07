#趁手工具先装上
install.packages("dplyr")
install.packages("tidyr")
#补装
install.packages("RSiena")
install.packages("RSienaTest")
install.packages("igraph")
#提取网络
df1 <- read.csv("~/Desktop/df1.csv")
View(df1)
t1df <- df1 %>%
  filter(point == "t1")
t2df <- df1 %>%
  filter(point == "t2")

#对于CBV 和CB进行重新赋值
#     小于等于1为0，代表无，大于1为1，代表有
#     运用ifelse()函数进行重新赋值
t1df$cbvictim <- ifelse(t1df$cbvictim > 1, 1, 0)
t1df$cbullying <- ifelse(t1df$cbullying > 1, 1, 0)
t2df$cbvictim <- ifelse(t2df$cbvictim > 1, 1, 0)
t2df$cbullying <- ifelse(t2df$cbullying > 1, 1, 0)

t2df <- t2df%>%
  mutate(class = as.numeric(substr(egoid, 1, 1)))
t1df <- t1df%>%
  rename(class = first_digit)

#转换为edgelist
#把数据变成from_id → to_id的格式(edgelist)
#提名对象(to_id)可能有NA，暂时先不过滤NA值
#但是需要清除重复值
t1edge <- t1df %>%
  select(class, egoid, starts_with("frd")) %>%
  gather(key = "varname", value = "fndid", -class, -egoid)%>%
  filter(!is.na(fndid)) %>%
  mutate(time     = "t1",
         same_class = class == varname)
t1edge

t1edge <- t1edge %>%
  distinct(egoid, fndid, .keep_all = TRUE)

t2edge <- t2df %>%
  select(class, egoid, starts_with("frd")) %>%
  gather(key = "varname", value = "fndid", -class, -egoid)%>%
  filter(!is.na(fndid)) %>%
  mutate(time     = "t1",
         same_class = class == varname)
t2edge

t2edge <- t2edge %>%
  distinct(egoid, fndid, .keep_all = TRUE)

#新增一列属性表示是否为跨班tie
t1edge <- t1edge %>%
  mutate(fndclass = as.numeric(substr(fndid, 1, 1)))
t2edge <- t2edge %>%
  mutate(fndclass = as.numeric(substr(fndid, 1, 1)))
t2edge <- t2edge[-c(837, 931), ]
t2edge$time <- "t2"

t1edge <- t1edge %>%
  mutate(same_class = class == fndclass)

t2edge <- t2edge %>%
  mutate(same_class = class == fndclass)

t1edge <- t1edge[, -c(7)]
t2edge <- t2edge[, -c(7)]

t2net <- t2edge[,-c(1, 3, 5, 6, 7) ]

t1net <- graph_from_data_frame(t1net,directed = TRUE)
t2net <- graph_from_data_frame(t2net,directed = TRUE)

t2matrix <- as_adjacency_matrix(t2net)

#把因变量也变成矩阵
# 使用dplyr包进行数据操作
library(dplyr)

# 提取cb和cbv数据到t1和t2列
t1victim <- t1df 
  t1victim <- t1victim[,-c(1, 4:49) ]

  t2victim <- t2df[,-c(1, 4:49) ]
  
t2cbullying <- t2df 
  t2cbullying <- t2cbullying[,-c(1, 3, 5:49) ]

t1cbullying <- t1cbullying%>%
    rename(t1 = cbullying)

t2cbullying <- t2cbullying%>%
  rename(t2 = cbullying)


# 更改列名为t1和t2，合并
colnames(t1cbullying) <- sub("cbullying", "t1_", colnames(t1cbullying))
colnames(t2cbullying) <- sub("cbullying", "t2_", colnames(t2cbullying))
cbullyingmatrix <- left_join(t1cbullying, t2cbullying, by = "egoid")

t1victim <- t1victim%>%
  rename(t1 = cbvictim)

t2victim <- t2victim%>%
  rename(t2 = cbvictim)

cbvictimmatrix <- left_join(t1victim, t2victim, by = "egoid")
colnames(t1victim) <- sub("cbvictim", "t1_", colnames(t1victim))
colnames(t2victim) <- sub("cbvictim", "t2_", colnames(t2victim))

cbvictimmatrix <- cbvictimmatrix %>%
  rename(t1 = t1_)

#把控制变量单独提出来
age <- t1df[,c(2, 29) ]
gender <- t1df[,c(2,30) ]
origin <- t1df[,c(2,31) ]

summary(t1df)
summary(t2df)
summary(age)

library(igraph)
edgesnew <- data.frame(
  from = c("A", "B", "B", "B", "D", "D", "G", "E"),
  to = c("B", "C", "D", "F", "F", "E", "F", "F")
)

graphnew <- graph.data.frame(edgesnew, directed = FALSE)

plot(t1graph,layout=layout_with_kk, vertex.label.dist = 2, vertex.label.cex = 0.7)







