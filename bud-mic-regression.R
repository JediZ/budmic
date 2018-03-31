# regression on bud mic conditions
#install.packages("broom")
library(broom)
#install.packages("bbmle")
library(bbmle)

#####
dat <- read.csv("budmic.csv")
colnames(dat)<-c("sample","id","temp","humi","fp","gp","as","tw","d10","d50","d90","smd","vmd","span")
head(dat)

summary(dat)

#####
m1 = glm(vmd~temp,data = dat)
m2 = glm(vmd~humi,data = dat)
m3 = glm(vmd~fp,data = dat)
m4 = glm(vmd~ gp,data = dat)
m5 = glm(vmd~ as,data = dat)
m6 = glm(vmd~ tw,data = dat)

m12 = glm(vmd~temp+humi,data = dat)
m13 = glm(vmd~temp+fp,data = dat)
m14 = glm(vmd~temp+gp,data = dat)
m15 = glm(vmd~ temp+as,data = dat)
m16 = glm(vmd~ temp+tw,data = dat)

m23 = glm(vmd~humi+fp,data = dat)
m24 = glm(vmd~humi+gp,data = dat)
m25 = glm(vmd~ humi+as,data = dat)
m26 = glm(vmd~ humi+tw,data = dat)

m34 = glm(vmd~fp+gp,data = dat)
m35 = glm(vmd~ fp+as,data = dat)
m36 = glm(vmd~ fp+tw,data = dat)

m45 = glm(vmd~ gp+as,data = dat)
m46 = glm(vmd~ gp+tw,data = dat)
m56 = glm(vmd~ as+tw,data = dat)

m123 = glm(vmd~temp+humi+fp,data = dat)
m124 = glm(vmd~temp+humi+gp,data = dat)
m125 = glm(vmd~temp+humi+as,data = dat)
m126 = glm(vmd~temp+humi+tw,data = dat)

m134 = glm(vmd~temp+fp+gp,data = dat)
m135 = glm(vmd~temp+fp+as,data = dat)
m136 = glm(vmd~temp+fp+tw,data = dat)

m145 = glm(vmd~temp+gp+as,data = dat)
m146 = glm(vmd~temp+gp+tw,data = dat)
m156 = glm(vmd~temp+as+tw,data = dat)

m234 = glm(vmd~humi+fp+gp,data = dat)
m235 = glm(vmd~humi+fp+as,data = dat)
m236 = glm(vmd~humi+fp+tw,data = dat)

m245 = glm(vmd~humi+gp+as,data = dat)
m246 = glm(vmd~humi+gp+tw,data = dat)
m256 = glm(vmd~humi+as+tw,data = dat)

m345 = glm(vmd~fp+gp+as,data = dat)
m346 = glm(vmd~fp+gp+tw,data = dat)
m356 = glm(vmd~fp+as+tw,data = dat)
m456 = glm(vmd~gp+as+tw,data = dat)

m1234 = glm(vmd~temp+humi+fp+gp,data = dat)
m1235 = glm(vmd~temp+humi+fp+as,data = dat)
m1236 = glm(vmd~temp+humi+fp+tw,data = dat)
m1245 = glm(vmd~temp+humi+gp+as,data = dat)
m1246 = glm(vmd~temp+humi+gp+tw,data = dat)
m1256 = glm(vmd~temp+humi+as+tw,data = dat)

m1345 = glm(vmd~temp+fp+gp+as,data = dat)
m1346 = glm(vmd~temp+fp+gp+tw,data = dat)
m1356 = glm(vmd~temp+fp+as+tw,data = dat)
m1456 = glm(vmd~temp+gp+as+tw,data = dat)

m2345 = glm(vmd~humi+fp+gp+as,data = dat)
m2346 = glm(vmd~humi+fp+gp+tw,data = dat)
m2356 = glm(vmd~humi+fp+as+tw,data = dat)
m2456 = glm(vmd~humi+gp+as+tw,data = dat)
m3456 = glm(vmd~fp+gp+as+tw,data = dat)

m12345 =glm(vmd~temp+humi+fp+gp+as,data = dat)
m12346 =glm(vmd~temp+humi+fp+gp+tw,data = dat)
m12356 =glm(vmd~temp+humi+fp+as+tw,data = dat)
m12456 =glm(vmd~temp+humi+gp+as+tw,data = dat)
m13456 =glm(vmd~temp+fp+gp+as+tw,data = dat)
m23456 =glm(vmd~humi+fp+gp+as+tw,data = dat)

m123456 = glm(vmd~temp+humi+fp+gp+as+tw,data = dat)

#####
model_list = list(
  #m1, m2, 
  m3,m4,m5,m6,
  #m12,m13,m14,m15,m16,
  #m23,m24,m25,m26,
  m34,m35,m36,m45,m46,m56,
  #m123,m124,m125,m126,m134,m135,m136,m145,m146,m156,
  #m234,m235,m236,m245,m246,m256,
  m345,m346,m356,m456,
  #m1234,m1235,m1236,m1245,m1246,m1256,
  #m1345,m1346,m1356,m1456,
  #m2345,m2346,m2356,m2456,
  m3456#,
  #m12345,m12346,m12356,m12456,m13456,m23456,
  #m123456
  )  

model.names <- c(
  #"temp", "humi", 
  "fp","gp","as","tw",
  #"temp+humi","temp+fp","temp+gp","temp+as","temp+tw",
  #"humi+fp","humi+gp","humi+as","humi+tw",
  "fp+gp","fp+as","fp+tw","gp+as","gp+tw","as+tw",
  #"temp+humi+fp","temp+humi+gp","temp+humi+as","temp+humi+tw",
  #"temp+fp+gp","temp+fp+as","temp+fp+tw",
  #"temp+gp+as","temp+gp+tw","temp+as+tw",
  #"humi+fp+gp","humi+fp+as","humi+fp+tw",
  #"humi+gp+as","humi+gp+tw","humi+as+tw",
  "fp+gp+as","fp+gp+tw","fp+as+tw","gp+as+tw",
  #"temp+humi+fp+gp","temp+humi+fp+as","temp+humi+fp+tw",
  #"temp+humi+gp+as","temp+humi+gp+tw","temp+humi+as+tw",
  #"temp+fp+gp+as","temp+fp+gp+tw","temp+fp+as+tw","temp+gp+as+tw",
  #"humi+fp+gp+as","humi+fp+gp+tw","humi+fp+as+tw","humi+gp+as+tw",
  "fp+gp+as+tw"#,
  #"temp+humi+fp+gp+as","temp+humi+fp+gp+tw","temp+humi+fp+as+tw",
  #"temp+humi+gp+as+tw","temp+fp+gp+as+tw","humi+fp+gp+as+tw",
  #"temp+humi+fp+gp+as+tw"
  ) 

######
summ.table <- do.call(rbind, lapply(model_list, broom::glance))

##
table.cols <- c("df.residual", "deviance", "AIC")
reported.table <- summ.table[table.cols]
names(reported.table) <- c("Resid. Df", "Resid. Dev", "AIC")

reported.table[['dAIC']] <-  with(reported.table, AIC - min(AIC))
reported.table[['weight']] <- with(reported.table, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table$AIC <- NULL
reported.table$weight <- round(reported.table$weight, 2)
reported.table$dAIC <- round(reported.table$dAIC, 1)

row.names(reported.table) <- model.names
## 
chosen <- reported.table$weight>=0.01
reported.table[chosen,]

#####
# Resid. Df Resid. Dev dAIC weight
# gp                 13  0.5104573  8.5   0.01
# gp+as              12  0.4194573  7.5   0.01
# gp+tw              12  0.3056496  2.8   0.14
# fp+gp+tw           11  0.3055574  4.8   0.05
# fp+as+tw           11  0.3728600  7.7   0.01
# gp+as+tw           11  0.2224401  0.0   0.56
# fp+gp+as+tw        10  0.2223596  2.0   0.21

# m123456 = glm(vmd~temp+humi+fp+gp+as+tw,data = dat)
##
head(summ.table)
model.names2 =  model.names[chosen]
summ.table2 = summ.table[chosen,]

reported.table2 <- bbmle::AICtab(m4, m45, m46,m346,m356,m456,m3456, weights = TRUE, sort = FALSE, mnames = model.names2)
reported.table2[["Resid. Dev"]]  <- summ.table2[["deviance"]] # get the deviance from broom'd table
reported.table2

models_chosen = model_list[chosen]
models_chosen_weight = reported.table2[["weight"]]

n=length(models_chosen_weight)
m=15
mavg.pred=rep(0,m)
for (i in 1:n){
  m.pred <- broom::augment(models_chosen[[i]])
  mavg.pred=m.pred$vmd*models_chosen_weight[i]+mavg.pred
}
mavg.pred

