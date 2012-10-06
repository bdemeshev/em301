# hometask

setwd('/home/boris/science/econometrix/em301/datasets/')

d=read.table('tvims2012_data.csv',header=TRUE,sep=",")

# check that the data is loaded correctly!
str(d)
head(d)
tail(d)


# estimate simple model
model1=glm(kr2~group+p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+name,data=d)
summary(model1)

# load name-sex correspondance table
n=read.table(file="names_sex.csv",sep=",")

# check data!
str(n)
head(n)
tail(n)

# add sex to the dataframe d
d$male=0

for (i in 1:nrow(d)) {
  d$male[i]=n$male_name[n$vec_names==d$name[i]] 
}

# more models...
model2=glm(kr2~group+p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+male,data=d)
summary(model2)


# Is popular name better than a rare name?
d$name.count=0
for (i in 1:nrow(d)) {
  d$name.count[i]=sum(d$name==d$name[i])
}

model3=glm(kr2~group+p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+male+name.count,data=d)
summary(model3)

