library(readr)
responses <- read_csv("D:/Capstone Project Covid_19/responses.csv")
View(responses)


x = rnorm(30)
shapiro.test(x)


y = runif(30)
shapiro.test(y)


x = rnorm(30)
ks.test(x, pnorm)


y = runif(30)
ks.test(y, pnorm)



weight.illinois = responses[responses$WEIGHT2 %in% 50:776,]
weight.illinois = weight.illinois[weight.illinois$ST %in% "IL",]
plot(density(weight.illinois$WEIGHT2), col="navy", lwd=3)
abline(v = 178, col="red", lwd=3)


weight.test = t.test(weight.illinois$WEIGHT2, mu=178)
print(weight.test)


weight.test = t.test(weight.illinois$WEIGHT2, mu=178, alternative="greater")
print(weight.test)


weight = responses[responses$WEIGHT2 %in% 50:776,]
weight.illinois = weight[weight$ST %in% "IL",]
weight.mississippi = weight[weight$ST %in% "MS",]
plot(density(weight.illinois$WEIGHT2), col="navy", lwd=3)
lines(density(weight.mississippi$WEIGHT2), col="red", lwd=3, add=T)
legend("topright", legend=c("Illinois", "Mississippi"), col=c("navy", "red"), lwd=3)


weight.test = t.test(weight.illinois$WEIGHT2, weight.mississippi$WEIGHT2, 
                     alternative="less")
print(weight.test)



drinking = responses[responses$AVEDRNK3 %in% 1:88, ]
drinking$AVEDRNK3[drinking$AVEDRNK3 == 88] = 0
hist(drinking$AVEDRNK3)


drinking.illlinois = drinking[drinking$ST %in% "IL",]
drinking.mississippi = drinking[drinking$ST %in% "MS",]
print(mean(drinking.illinois$AVEDRNK3))
print(mean(drinking.mississippi$AVEDRNK3))


drinking.test = wilcox.test(drinking.il$AVEDRNK3, drinking.ms$AVEDRNK3, 
                            alternative="less")
print(drinking.test)


library(weight)

drinking = responses[responses$AVEDRNK3 %in% 1:88, ]
drinking$AVEDRNK3[drinking$AVEDRNK3 == 88] = 0
drinking.illlinois = drinking[drinking$ST %in% "IL",]
drinking.mississippi = drinking[drinking$ST %in% "MS",]
drinking.test = wtd.t.test(x = drinking.illinois$AVEDRNK3, 
                           y = drinking.mississippi$AVEDRNK3,
                           weight=drinking.illinois$X_LLCPWT, 
                           weighty = drinking.mississippi$X_LLCPWT)

print(drinking.test)



smoking = responses
smoking$SMOKDAY2 = ifelse(smoking$SMOKDAY2 %in% 1:2, 1, 0)
smoking.illinois = table(smoking[smoking$ST %in% "IL", "SMOKDAY2"])
print(smoking.illinois * 100 / sum(smoking.illinois))

smoking.test = chisq.test(smoking.illinois, p=c(0.777, 0.223))
print(smoking.test)



smoking = responses[responses$SMOKDAY2 %in% c(1,2,3,NA),]
smoking$SMOKDAY2[is.na(smoking$SMOKDAY2)] = 3
smoking.table = table(smoking[smoking$ST %in% c("IL", "MS"), c("ST", "SMOKDAY2")])
print(smoking.table)
plot(smoking.table)


smoking.test = chisq.test(smoking.table)
print(smoking.test)





library(weights)
smoking = responses[responses$SMOKDAY2 %in% c(1,2,3,NA),]
smoking$SMOKDAY2[is.na(smoking$SMOKDAY2)] = 3
smoking = smoking[smoking$ST %in% c("IL", "MS"),]
smoking.test = wtd.chi.sq(var1 = smoking$ST, var2 = smoking$SMOKDAY2, 
                          weight = smoking$X_LLCPWT)
print(smoking.test)



weight.illinois = responses[(responses$WEIGHT2 %in% 50:776) & 
                              (responses$INCOME2 %in% 1:8) & 
                              (responses$ST %in% "IL"),]
weight.list = aggregate(weight.illinois$WEIGHT2, by=list(weight.illinois$INCOME2), FUN=mean)
x = barplot(weight.list$x, names.arg=weight.list$Group.1, las=1)
text(x, 20, round(weight.list$x), cex=0.8)


model = aov(WEIGHT2 ~ INCOME2, data = weight.illinois)
summary(model)


weight.urban = responses[(responses$WEIGHT2 %in% 50:776) &
                           (responses$ST %in% c("NY","IL","CA")),]
boxplot(WEIGHT2 ~ ST, data = weight.urban)


model = kruskal.test(WEIGHT2 ~ ST, data = weight.urban)
print(model)


library(readr)
columns <- read_csv("D:/Capstone Project Covid_19/2020-column-layout.csv")
View(columns)
columns$File_Width = sapply(1:nrow(columns), function(y) ifelse(y < nrow(columns), 
                                                               columns$Starting_Column[y + 1] - columns$Starting_Column[y], 1))
columns = columns[columns$File_Width > 0,]



responses <- read_csv("D:/Capstone Project Covid_19/LLCP2020.ASC" widths = columns$File_Width, col.names = columns$Variable_Name)


name_list = c("X_STATE", "X_URBSTAT", "X_AGEG5YR", "DISPCODE", "NUMADULT", "SEXVAR", "GENHLTH",
              "PHYSHLTH", "MENTHLTH", "HLTHPLN1", "EXERANY2", "SLEPTIM1", "CVDINFR4",
              "CVDCRHD4", "CVDSTRK3", "ASTHMA3", "CHCSCNCR", "CHCOCNCR", "CHCCOPD2",
              "HAVARTH4", "ADDEPEV3", "CHCKDNY2", "DIABETE4", "DIABAGE3", "LASTDEN4",
              "RMVTETH4", "MARITAL", "EDUCA", "RENTHOM1", "VETERAN3", "EMPLOY1", "CHILDREN",
              "INCOME2", "PREGNANT", "WEIGHT2", "HTIN4", "DEAF", "BLIND", "SMOKDAY2",
              "STOPSMK2", "AVEDRNK3", "DRNK3GE5", "SEATBELT", "HADMAM", "PSATEST1",
              "COLNSCPY", "HIVTST7", "HIVRISK5", "X_LLCPWT")

responses = responses[, name_list]
