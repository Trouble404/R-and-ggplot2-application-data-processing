#
library(ggplot2)
# set working directory
setwd("E:/Data Science Software/Github/Clone/comp6235_1718_coursework-1")
# reading data
Fishdata <- read.table("fish.txt", header = F)
# rename of input data
names(Fishdata)[1] <- "time"
names(Fishdata)[2] <- "num"
# obtain and save summary information of fish.txt
information <- summary(Fishdata)
write.table(information,"information.txt",row.names=F)
# variable names from the queue data (time and num) will be available for use
attach(Fishdata)
# display time #
# calculate bin size with Freedman–Diaconis rule
time_Q1 <- 4.885
time_Q3 <- 13.220
time_bins <- 2*((time_Q3-time_Q1)/200^(1/3))
num_Q1 <- 0.9375
num_Q3 <- 2.7375
num_bins <- 2*((num_Q3-num_Q1)/200^(1/3))
#set x axis
hours<-c("1", "2","3", "4" , "5" ,"6", "7", "8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","(h)")
num1<-c("1(kg)","2(kg)","3(kg)","4(kg)","5(kg)")
num2<-c("1(kg)","2(kg)","3(kg)","4(kg)")
num3<-c("1(kg)","2(kg)","3(kg)")
#plot hist and density of time
time1 <- time
time_plot <- data.frame(x = time1)

time_den <- ggplot(time_plot, aes(x=x,y=..density..)) + geom_histogram(fill = "lightblue",colour="black",binwidth = 1)+xlim(hours)+ labs(x = "24 hours", y = "density", title = "Density and Historgram of fishing time")+geom_density(colour="red")
time_den <- time_den + geom_text(aes(x = 0.5, y = 0.047), label = "9", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 1.5, y = 0.067), label = "13", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 2.5, y = 0.052), label = "10", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 3.5, y = 0.037), label = "7", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 4.5, y = 0.072), label = "14", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 5.5, y = 0.062), label = "12", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 6.5, y = 0.057), label = "11", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 7.5, y = 0.062), label = "12", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 8.5, y = 0.067), label = "13", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 9.5, y = 0.037), label = "7", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 10.5, y = 0.077), label = "15", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 11.5, y = 0.042), label = "8", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 12.5, y = 0.082), label = "16", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 13.5, y = 0.037), label = "7", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 14.5, y = 0.032), label = "6", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 15.5, y = 0.052), label = "10", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 16.5, y = 0.027), label = "5", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 17.5, y = 0.047), label = "9", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 18.5, y = 0.032), label = "6", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 19.5, y = 0.027), label = "5", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 20.5, y = 0.007), label = "1", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 21.5, y = 0.017), label = "3", colour="black",size = 4)
time_den <- time_den + geom_text(aes(x = 23.5, y = 0.007), label = "1", colour="black",size = 4)
time_den <- time_den + geom_vline(xintercept = 9.38835,size=1,colour="#BB0000", linetype="dashed")
time_den <- time_den + geom_vline(xintercept = 8.950,size=1,colour="#009E73", linetype="dashed")
time_den <- time_den + geom_text(aes(x = 20, y = 0.075), label = "red line is the mean value of time: 9.388", colour="red",size = 3)
time_den <- time_den + geom_text(aes(x = 20, y = 0.07), label = "green line is the median value of time: 8.950", colour="#009E73",size = 3)
time_den <- time_den + geom_text(aes(x = 20, y = 0.08), label = "frequency numbers on the histogram", colour="black",size = 3)
ggsave("time_density.png", time_den,  dpi=600) 
# #
time_den <- ggplot(time_plot, aes(x=x,y=..density..)) + geom_histogram(fill = "lightblue",colour="black",binwidth = time_bins)+xlim(hours)+ labs(x = "24 hours", y = "density", title = "Density and Historgram of fishing time based on Freedman-Diaconis rule")+geom_density(colour="red")
ggsave("time_density_Freedman.png", time_den, dpi=600) 
#plot hist and density of catch
# calculate frequency of num
num1 <- num[num<=0.6155913]
num_f1 <- length(num1)
num2 <- num[num<=2*0.6155913]
num_f2 <- length(num2) - num_f1
num3 <- num[num<=3*0.6155913]
num_f3 <- length(num3) - num_f2 - num_f1
num4 <- num[num<=4*0.6155913]
num_f4 <- length(num4) - num_f3 - num_f2 - num_f1
num5 <- num[num<=5*0.6155913]
num_f5 <- length(num5) - num_f4 - num_f3 - num_f2 - num_f1
num6 <- num[num<=6*0.6155913]
num_f6 <- length(num6) - num_f5 - num_f4 - num_f3 - num_f2 - num_f1
num7 <- num[num<=7*0.6155913]
num_f7 <- length(num7) - num_f6 - num_f5 - num_f4 - num_f3 - num_f2 - num_f1
#
hours<-c("1", "2","3", "4" , "5" ,"6", "7", "8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","(h)")
num1<-c("1(kg)","2(kg)","3(kg)","4(kg)","5(kg)")
num2<-c("1(kg)","2(kg)","3(kg)","4(kg)")
num3<-c("1(kg)","2(kg)","3(kg)")
#
catch1 <- num
catch_plot <- data.frame(x = catch1)
catch_den <- ggplot(catch_plot, aes(x=x,y=..density..)) + geom_histogram(fill = "lightblue",colour="black",binwidth = num_bins)+xlim(num1)+ labs(x = "fish weight", y = "density", title = "Density and Historgram of fishing weight based on Freedman-Diaconis rule")+geom_density(colour="red")
catch_den <- catch_den + geom_text(aes(x = 0.30779565, y = 0.245), label = "29", colour="black",size = 4)
catch_den <- catch_den + geom_text(aes(x = 0.92338695, y = 0.36), label = "43", colour="black",size = 4)
catch_den <- catch_den + geom_text(aes(x = 1.53897825, y = 0.2525), label = "30", colour="black",size = 4)
catch_den <- catch_den + geom_text(aes(x = 2.15456955, y = 0.2355), label = "28", colour="black",size = 4)
catch_den <- catch_den + geom_text(aes(x = 2.77016085, y = 0.293), label = "35", colour="black",size = 4)
catch_den <- catch_den + geom_text(aes(x = 3.38575215, y = 0.245), label = "29", colour="black",size = 4)
catch_den <- catch_den + geom_text(aes(x = 4.00134345, y = 0.056), label = "6", colour="black",size = 4)
catch_den <- catch_den + geom_vline(xintercept = 1.8431,size=1,colour="#BB0000", linetype="dashed")
catch_den <- catch_den + geom_vline(xintercept = 1.8250,size=1,colour="#009E73", linetype="dashed")
catch_den <- catch_den + geom_text(aes(x = 4, y = 0.36), label = "red line is the mean value of time: 1.843", colour="red",size = 3)
catch_den <- catch_den + geom_text(aes(x = 4, y = 0.32), label = "green line is the median value of time: 1.825", colour="#009E73",size = 3)
catch_den <- catch_den + geom_text(aes(x = 4, y = 0.4), label = "frequency numbers on the histogram", colour="black",size = 3)

ggsave("catch_density_Freedman.png", catch_den,  dpi=600) 
# plot line chart#
# sort the data
data_fish <- cbind(time,num)
data_sort <- data_fish[order(data_fish[,1]),]

x_value <- data.frame(x = data_sort[,1])
y_value <- data.frame(y = data_sort[,2])

line_data<-data.frame(x_value,y_value)

names(line_data)[1] <- "hour"
names(line_data)[2] <- "kg"
a <- ggplot(line_data,aes(x=hour,y=kg))+geom_line(size = 0.2, colour = "orange")+geom_point(size = 1.5, shape = 21,fill = "white")+xlim(hours)+ ylim(num2) +labs(x = "24 hours", y = "weight of fish", title = "Line chart of fishing time and weight")
a <- a +  geom_hline(aes(yintercept=1.84305),size=0.5,colour="#BB0000", linetype="solid") + geom_hline(aes(yintercept=1.693636289),size=0.5,colour="#BB0000", linetype="dashed") + geom_hline(aes(yintercept=1.992463711),size=0.5,colour="#BB0000", linetype="dashed")
a <- a + geom_text(aes(x = 18, y = 4.5), label = "red line area is the 95% Confidence intervals of weight mean(1.69~1.99)", colour="#BB0000",size = 3.3)
a <- a + geom_vline(xintercept = 9.38835,size=0.5,colour="#009E73", linetype="solid")+geom_vline(xintercept = 8.604370685,size=0.5,colour="#009E73", linetype="dashed")+geom_vline(xintercept = 10.17232932,size=0.5,colour="#009E73", linetype="dashed")
a <- a + geom_text(aes(x = 18, y = 4.2), label = "green line area is the 95% Confidence intervals of time mean(8.60~10.17)", colour="#009E73",size = 3.3)
ggsave("plot.png", a, width = 8.49, height = 5.46, dpi=600) 



# sample
# mean fo time
x1 <- mean(data_sort[1:9])
x2 <- mean(data_sort[10:22])
x3 <- mean(data_sort[23:32])
x4 <- mean(data_sort[33:39])
x5 <- mean(data_sort[40:53])
x6 <- mean(data_sort[54:65])
x7 <- mean(data_sort[66:76])
x8 <- mean(data_sort[77:88])
x9 <- mean(data_sort[89:101])
x10 <- mean(data_sort[102:108])
x11 <- mean(data_sort[109:123])
x12 <- mean(data_sort[124:131])
x13 <- mean(data_sort[132:147])
x14 <- mean(data_sort[148:154])
x15 <- mean(data_sort[155:160])
x16 <- mean(data_sort[161:170])
x17 <- mean(data_sort[171:175])
x18 <- mean(data_sort[176:184])
x19 <- mean(data_sort[185:190])
x20 <- mean(data_sort[191:195])
x21 <- mean(data_sort[196:196])
x22 <- mean(data_sort[197:199])
x24 <- mean(data_sort[200:200])
tm<- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x24)
# sd of time
s1 <- sd(data_sort[1:9])
s2 <- sd(data_sort[10:22])
s3 <- sd(data_sort[23:32])
s4 <- sd(data_sort[33:39])
s5 <- sd(data_sort[40:53])
s6 <- sd(data_sort[54:65])
s7 <- sd(data_sort[66:76])
s8 <- sd(data_sort[77:88])
s9 <- sd(data_sort[89:101])
s10 <- sd(data_sort[102:108])
s11 <- sd(data_sort[109:123])
s12 <- sd(data_sort[124:131])
s13 <- sd(data_sort[132:147])
s14 <- sd(data_sort[148:154])
s15 <- sd(data_sort[155:160])
s16 <- sd(data_sort[161:170])
s17 <- sd(data_sort[171:175])
s18 <- sd(data_sort[176:184])
s19 <- sd(data_sort[185:190])
s20 <- sd(data_sort[191:195])
s21 <- 0
s22 <- sd(data_sort[197:199])
s24 <- 0
ts <- rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s24)
# 95% CI of time
upper_limit1 <- x1 + (s1*1.96/sqrt(9))
lower_limit1 <- x1 - (s1*1.96/sqrt(9))
lower_limit2 <- x2 - (s2*1.96/sqrt(13))
upper_limit2 <- x2 + (s2*1.96/sqrt(13))
lower_limit3 <- x3 - (s3*1.96/sqrt(10))
upper_limit3 <- x3 + (s3*1.96/sqrt(10))
upper_limit4 <- x4 + (s4*1.96/sqrt(7))
lower_limit4 <- x4 - (s4*1.96/sqrt(7))
lower_limit5 <- x5 - (s5*1.96/sqrt(14))
upper_limit5 <- x5 + (s5*1.96/sqrt(14))
upper_limit6 <- x6 + (s6*1.96/sqrt(12))
lower_limit6 <- x6 - (s6*1.96/sqrt(12))
lower_limit7 <- x7 - (s7*1.96/sqrt(11))
upper_limit7 <- x7 + (s7*1.96/sqrt(11))
upper_limit8 <- x8 + (s8*1.96/sqrt(12))
lower_limit8 <- x8 - (s8*1.96/sqrt(12))
lower_limit9 <- x9 - (s9*1.96/sqrt(13))
upper_limit9 <- x9 + (s9*1.96/sqrt(13))
upper_limit10 <- x10 + (s10*1.96/sqrt(7))
lower_limit10 <- x10 - (s10*1.96/sqrt(7))
lower_limit11 <- x11 - (s11*1.96/sqrt(15))
upper_limit11 <- x11 + (s11*1.96/sqrt(15))
upper_limit12 <- x12 + (s12*1.96/sqrt(8))
lower_limit12 <- x12 - (s12*1.96/sqrt(8))
lower_limit13 <- x13 - (s13*1.96/sqrt(16))
upper_limit13 <- x13 + (s13*1.96/sqrt(16))
upper_limit14 <- x14 + (s14*1.96/sqrt(7))
lower_limit14 <- x14 - (s14*1.96/sqrt(7))
lower_limit15 <- x15 - (s15*1.96/sqrt(6))
upper_limit15 <- x15 + (s15*1.96/sqrt(6))
upper_limit16 <- x16 + (s16*1.96/sqrt(10))
lower_limit16 <- x16 - (s16*1.96/sqrt(10))
lower_limit17 <- x17 - (s17*1.96/sqrt(5))
upper_limit17 <- x17 + (s17*1.96/sqrt(5))
upper_limit18 <- x18 + (s18*1.96/sqrt(9))
lower_limit18 <- x18 - (s18*1.96/sqrt(9))
lower_limit19 <- x19 - (s19*1.96/sqrt(6))
upper_limit19 <- x19 + (s19*1.96/sqrt(6))
upper_limit20 <- x20 + (s20*1.96/sqrt(5))
lower_limit20 <- x20 - (s20*1.96/sqrt(5))
lower_limit21 <- 0
upper_limit21 <- 0
lower_limit22 <- x22 - (s22*1.96/sqrt(3))
upper_limit22 <- x22 + (s22*1.96/sqrt(3))
lower_limit24 <- 0
upper_limit24 <- 0
tul <- rbind(upper_limit1,upper_limit2,upper_limit3,upper_limit4,upper_limit5,upper_limit6,upper_limit7,upper_limit8,upper_limit9,upper_limit10,upper_limit11,upper_limit12,upper_limit13,upper_limit14,upper_limit15,upper_limit16,upper_limit17,upper_limit18,upper_limit19,upper_limit20,upper_limit21,upper_limit22,upper_limit24)
tll <- rbind(lower_limit1,lower_limit2,lower_limit3,lower_limit4,lower_limit5,lower_limit6,lower_limit7,lower_limit8,lower_limit9,lower_limit10,lower_limit11,lower_limit12,lower_limit13,lower_limit14,lower_limit15,lower_limit16,lower_limit17,lower_limit18,lower_limit19,lower_limit20,lower_limit21,lower_limit22,lower_limit24)
# calculate CI of time
CI1 <- upper_limit1 - lower_limit1
CI2 <- upper_limit2 - lower_limit2
CI3 <- upper_limit3 - lower_limit3
CI4 <- upper_limit4 - lower_limit4
CI5 <- upper_limit5 - lower_limit5
CI6 <- upper_limit6 - lower_limit6
CI7 <- upper_limit7 - lower_limit7
CI8 <- upper_limit8 - lower_limit8
CI9 <- upper_limit9 - lower_limit9
CI10 <- upper_limit10 - lower_limit10
CI11 <- upper_limit11 - lower_limit11
CI12 <- upper_limit12 - lower_limit12
CI13 <- upper_limit13 - lower_limit13
CI14 <- upper_limit14 - lower_limit14
CI15 <- upper_limit15 - lower_limit15
CI16 <- upper_limit16 - lower_limit16
CI17 <- upper_limit17 - lower_limit17
CI18 <- upper_limit18 - lower_limit18
CI19 <- upper_limit19 - lower_limit19
CI20 <- upper_limit20 - lower_limit20
CI21 <- upper_limit21 - lower_limit21
CI22 <- upper_limit22 - lower_limit22
CI24 <- upper_limit24 - lower_limit24
tci <- rbind(CI1,CI2,CI3,CI4,CI5,CI6,CI7,CI8,CI9,CI10,CI11,CI12,CI13,CI14,CI15,CI16,CI17,CI18,CI19,CI20,CI21,CI22,CI24)

# data of 24 hours point
time_24 <- cbind(tm,ts,tul,tll,tci)
# mean of weight
x1 <- mean(data_sort[201:209])
x2 <- mean(data_sort[210:222])
x3 <- mean(data_sort[223:232])
x4 <- mean(data_sort[233:239])
x5 <- mean(data_sort[240:253])
x6 <- mean(data_sort[254:265])
x7 <- mean(data_sort[266:276])
x8 <- mean(data_sort[277:288])
x9 <- mean(data_sort[289:301])
x10 <- mean(data_sort[302:308])
x11 <- mean(data_sort[309:323])
x12 <- mean(data_sort[324:331])
x13 <- mean(data_sort[332:347])
x14 <- mean(data_sort[348:354])
x15 <- mean(data_sort[355:360])
x16 <- mean(data_sort[361:370])
x17 <- mean(data_sort[371:375])
x18 <- mean(data_sort[376:384])
x19 <- mean(data_sort[385:390])
x20 <- mean(data_sort[391:395])
x21 <- mean(data_sort[396:396])
x22 <- mean(data_sort[397:399])
x24 <- mean(data_sort[400:400])
wm<- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x24)
# sd of weight
s1 <- sd(data_sort[201:209])
s2 <- sd(data_sort[210:222])
s3 <- sd(data_sort[223:232])
s4 <- sd(data_sort[233:239])
s5 <- sd(data_sort[240:253])
s6 <- sd(data_sort[254:265])
s7 <- sd(data_sort[266:276])
s8 <- sd(data_sort[277:288])
s9 <- sd(data_sort[289:301])
s10 <- sd(data_sort[302:308])
s11 <- sd(data_sort[309:323])
s12 <- sd(data_sort[324:331])
s13 <- sd(data_sort[332:347])
s14 <- sd(data_sort[348:354])
s15 <- sd(data_sort[355:360])
s16 <- sd(data_sort[361:370])
s17 <- sd(data_sort[371:375])
s18 <- sd(data_sort[376:384])
s19 <- sd(data_sort[385:390])
s20 <- sd(data_sort[391:395])
s21 <- 0
s22 <- sd(data_sort[397:399])
s24 <- 0
ws <- rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s24)
# 95% CI of weight
upper_limit1 <- x1 + (s1*1.96/sqrt(9))
lower_limit1 <- x1 - (s1*1.96/sqrt(9))
lower_limit2 <- x2 - (s2*1.96/sqrt(13))
upper_limit2 <- x2 + (s2*1.96/sqrt(13))
lower_limit3 <- x3 - (s3*1.96/sqrt(10))
upper_limit3 <- x3 + (s3*1.96/sqrt(10))
upper_limit4 <- x4 + (s4*1.96/sqrt(7))
lower_limit4 <- x4 - (s4*1.96/sqrt(7))
lower_limit5 <- x5 - (s5*1.96/sqrt(14))
upper_limit5 <- x5 + (s5*1.96/sqrt(14))
upper_limit6 <- x6 + (s6*1.96/sqrt(12))
lower_limit6 <- x6 - (s6*1.96/sqrt(12))
lower_limit7 <- x7 - (s7*1.96/sqrt(11))
upper_limit7 <- x7 + (s7*1.96/sqrt(11))
upper_limit8 <- x8 + (s8*1.96/sqrt(12))
lower_limit8 <- x8 - (s8*1.96/sqrt(12))
lower_limit9 <- x9 - (s9*1.96/sqrt(13))
upper_limit9 <- x9 + (s9*1.96/sqrt(13))
upper_limit10 <- x10 + (s10*1.96/sqrt(7))
lower_limit10 <- x10 - (s10*1.96/sqrt(7))
lower_limit11 <- x11 - (s11*1.96/sqrt(15))
upper_limit11 <- x11 + (s11*1.96/sqrt(15))
upper_limit12 <- x12 + (s12*1.96/sqrt(8))
lower_limit12 <- x12 - (s12*1.96/sqrt(8))
lower_limit13 <- x13 - (s13*1.96/sqrt(16))
upper_limit13 <- x13 + (s13*1.96/sqrt(16))
upper_limit14 <- x14 + (s14*1.96/sqrt(7))
lower_limit14 <- x14 - (s14*1.96/sqrt(7))
lower_limit15 <- x15 - (s15*1.96/sqrt(6))
upper_limit15 <- x15 + (s15*1.96/sqrt(6))
upper_limit16 <- x16 + (s16*1.96/sqrt(10))
lower_limit16 <- x16 - (s16*1.96/sqrt(10))
lower_limit17 <- x17 - (s17*1.96/sqrt(5))
upper_limit17 <- x17 + (s17*1.96/sqrt(5))
upper_limit18 <- x18 + (s18*1.96/sqrt(9))
lower_limit18 <- x18 - (s18*1.96/sqrt(9))
lower_limit19 <- x19 - (s19*1.96/sqrt(6))
upper_limit19 <- x19 + (s19*1.96/sqrt(6))
upper_limit20 <- x20 + (s20*1.96/sqrt(5))
lower_limit20 <- x20 - (s20*1.96/sqrt(5))
lower_limit21 <- 1.57
upper_limit21 <- 1.57
lower_limit22 <- x22 - (s22*1.96/sqrt(3))
upper_limit22 <- x22 + (s22*1.96/sqrt(3))
lower_limit24 <- 1.34
upper_limit24 <- 1.34
wul <- rbind(upper_limit1,upper_limit2,upper_limit3,upper_limit4,upper_limit5,upper_limit6,upper_limit7,upper_limit8,upper_limit9,upper_limit10,upper_limit11,upper_limit12,upper_limit13,upper_limit14,upper_limit15,upper_limit16,upper_limit17,upper_limit18,upper_limit19,upper_limit20,upper_limit21,upper_limit22,upper_limit24)
wll <- rbind(lower_limit1,lower_limit2,lower_limit3,lower_limit4,lower_limit5,lower_limit6,lower_limit7,lower_limit8,lower_limit9,lower_limit10,lower_limit11,lower_limit12,lower_limit13,lower_limit14,lower_limit15,lower_limit16,lower_limit17,lower_limit18,lower_limit19,lower_limit20,lower_limit21,lower_limit22,lower_limit24)
# calculate CI of weight
CI1 <- upper_limit1 - lower_limit1
CI2 <- upper_limit2 - lower_limit2
CI3 <- upper_limit3 - lower_limit3
CI4 <- upper_limit4 - lower_limit4
CI5 <- upper_limit5 - lower_limit5
CI6 <- upper_limit6 - lower_limit6
CI7 <- upper_limit7 - lower_limit7
CI8 <- upper_limit8 - lower_limit8
CI9 <- upper_limit9 - lower_limit9
CI10 <- upper_limit10 - lower_limit10
CI11 <- upper_limit11 - lower_limit11
CI12 <- upper_limit12 - lower_limit12
CI13 <- upper_limit13 - lower_limit13
CI14 <- upper_limit14 - lower_limit14
CI15 <- upper_limit15 - lower_limit15
CI16 <- upper_limit16 - lower_limit16
CI17 <- upper_limit17 - lower_limit17
CI18 <- upper_limit18 - lower_limit18
CI19 <- upper_limit19 - lower_limit19
CI20 <- upper_limit20 - lower_limit20
CI21 <- upper_limit21 - lower_limit21
CI22 <- upper_limit22 - lower_limit22
CI24 <- upper_limit24 - lower_limit24
wci <- rbind(CI1,CI2,CI3,CI4,CI5,CI6,CI7,CI8,CI9,CI10,CI11,CI12,CI13,CI14,CI15,CI16,CI17,CI18,CI19,CI20,CI21,CI22,CI24)

weight_24 <- cbind(wm,ws,wul,wll,wci)
# approximate value
appr1 <- 2.03
appr2 <- 2.10
appr3 <- 2.00
appr4 <- 1.94
appr5 <- 1.85
appr6 <- 2.37
appr7 <- 1.89
appr8 <- 1.72
appr9 <- 2.30
appr10 <- 1.60
appr11 <- 1.59
appr12 <- 1.32
appr13 <- 1.79
appr14 <- 1.67
appr15 <- 1.24
appr16 <- 2.09
appr17 <- 1.67
appr18 <- 1.80
appr19 <- 1.07
appr20 <- 1.74
appr21 <- 1.57
appr23 <- 2.37
appr24 <- 1.34
awm <- rbind(appr1,appr2,appr3,appr4,appr5,appr6,appr7,appr8,appr9,appr10,appr11,appr12,appr13,appr14,appr15,appr16,appr17,appr18,appr19,appr20,appr21,appr23,appr24)
# combine
all_data <- cbind(time_24,weight_24,awm)





# obtain each line
time_mean <- data.frame(x = all_data[,1])
time_sd <- data.frame(x = all_data[,2])
time_upper <- data.frame(x = all_data[,3])
time_lower <- data.frame(x = all_data[,4])
time_ci <- data.frame(x = all_data[,5])
weight_mean <- data.frame(y = all_data[,6])
weight_sd <- data.frame(y = all_data[,7])
weight_upper <- data.frame(y = all_data[,8])
weight_lower <- data.frame(y = all_data[,9])
weight_ci <- data.frame(y = all_data[,10])
weight_appr <- data.frame(y = all_data[,11])

line_data<-data.frame(time_mean,time_sd,time_lower,time_upper,time_ci,weight_mean,weight_sd,weight_lower,weight_upper,weight_ci,weight_appr)

names(line_data)[1] <- "MeanTime"
names(line_data)[2] <- "SDtime"
names(line_data)[3] <- "LowerBandTime"
names(line_data)[4] <- "UpperBandTime"
names(line_data)[5] <- "TimeCI"
names(line_data)[6] <- "MeanWeight"
names(line_data)[7] <- "SDweight"
names(line_data)[8] <- "LowerBandWight"
names(line_data)[9] <- "UppoerBandWeight"
names(line_data)[10] <- "WeightCI"
names(line_data)[11] <- "WeightAppr"



b <- ggplot(line_data, aes(x = MeanTime, y= MeanWeight )) + geom_ribbon(aes(ymin = LowerBandWight, ymax=UppoerBandWeight), alpha = 0.2) + geom_line(size = 1, colour = "orange") + geom_point(size = 2, shape = 21,fill = "white")+xlim(hours)+ ylim(num2) +labs(x = "Fishing Time: 24 hours", y = "Weight of fish", title = "Line chart of fishing time and weight(Hour by Hour-sample)") 
b <- b + geom_text(aes(label = WeightAppr), vjust = -1, colour = 'black', size = 2)
b <- b + geom_text(aes(x = 12, y = 4.1), label = "The grey area is the 95% confidence intervals of fishing weight", colour="#BB0000",size = 4)
b <- b + geom_text(aes(x = 12, y = 3.8), label = "The 23 white points are the mean value of each hour range", colour="#BB0000",size = 4)
b <- b + geom_text(aes(x = 12, y = 3.5), label = "The area between | | is the 95% confidence intervals of fishing time", colour="#BB0000",size = 4)
b <- b + geom_text(aes(x = 0.174882462, y = 1.8025556), label = "|",size = 3) + geom_text(aes(x = 0.620673094, y = 1.8025556), label = "|",size = 3)
b <- b + geom_text(aes(x = 1.33510914, y = 1.9), label = "|",size = 3) + geom_text(aes(x = 1.618737014, y = 1.9), label = "|",size = 3)
b <- b + geom_text(aes(x = 2.312758383, y = 1.798), label = "|",size = 3) + geom_text(aes(x = 2.727241617, y = 1.798), label = "|",size = 3)
b <- b + geom_text(aes(x = 3.467462843, y = 1.7371), label = "|",size = 3) + geom_text(aes(x = 3.766822871, y = 1.7371), label = "|",size = 3)
b <- b + geom_text(aes(x = 4.230359568, y = 1.652), label = "|",size = 3) + geom_text(aes(x = 4.599640432, y = 1.652), label = "|",size = 3)
b <- b + geom_text(aes(x = 5.371161355, y = 2.17), label = "|",size = 3) + geom_text(aes(x = 5.675505312, y = 2.17), label = "|",size = 3)
b <- b + geom_text(aes(x = 6.223235538, y = 1.69), label = "|",size = 3) + geom_text(aes(x = 6.649491735, y = 1.69), label = "|",size = 3)
b <- b + geom_text(aes(x = 7.292973451, y = 1.518), label = "|",size = 3) + geom_text(aes(x = 7.563693216, y = 1.518), label = "|",size = 3)
b <- b + geom_text(aes(x = 8.323719121, y = 2.095), label = "|",size = 3) + geom_text(aes(x = 8.633203955, y = 2.095), label = "|",size = 3)
b <- b + geom_text(aes(x = 9.489779387, y = 1.395), label = "|",size = 3) + geom_text(aes(x = 9.861649185, y = 1.395), label = "|",size = 3)
b <- b + geom_text(aes(x = 10.21861229, y = 1.387), label = "|",size = 3) + geom_text(aes(x = 10.50005437, y = 1.387), label = "|",size = 3)
b <- b + geom_text(aes(x = 11.28987753, y = 1.116), label = "|",size = 3) + geom_text(aes(x = 11.71262247, y = 1.116), label = "|",size = 3)
b <- b + geom_text(aes(x = 12.37558788, y = 1.591), label = "|",size = 3) + geom_text(aes(x = 12.73066212, y = 1.591), label = "|",size = 3)
b <- b + geom_text(aes(x = 13.12204333, y = 1.465), label = "|",size = 3) + geom_text(aes(x = 13.65224239, y = 1.465), label = "|",size = 3)
b <- b + geom_text(aes(x = 14.28543953, y = 1.041), label = "|",size = 3) + geom_text(aes(x = 14.73456047, y = 1.041), label = "|",size = 3)
b <- b + geom_text(aes(x = 15.21650484, y = 1.809), label = "|",size = 3) + geom_text(aes(x = 15.51749516, y = 1.809), label = "|",size = 3)
b <- b + geom_text(aes(x = 16.23195996, y = 1.468), label = "|",size = 3) + geom_text(aes(x = 16.79204004, y = 1.468), label = "|",size = 3)
b <- b + geom_text(aes(x = 17.19981587, y = 1.600), label = "|",size = 3) + geom_text(aes(x = 17.57573968, y = 1.600), label = "|",size = 3)
b <- b + geom_text(aes(x = 18.24319150, y = 0.868), label = "|",size = 3) + geom_text(aes(x = 18.73014183, y = 0.868), label = "|",size = 3)
b <- b + geom_text(aes(x = 19.35173313, y = 1.538), label = "|",size = 3) + geom_text(aes(x = 19.89226687, y = 1.538), label = "|",size = 3)
b <- b + geom_text(aes(x = 20.89362916, y = 2.170), label = "|",size = 3) + geom_text(aes(x = 21.50637084, y = 2.170), label = "|",size = 3)



ggsave("plot_time_weight.png", b, width = 8.49, height = 5.46,dpi=600) 