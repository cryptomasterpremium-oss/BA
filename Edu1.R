A <- 20

A
w = 45
e= 67.5
s= "Amal"
r= TRUE

class(w)
class(e)
class(s)
class(r)

#Vectors
#One_Dimensional_Array
vec_num1 = c(1:5)
vec_num2 = c(20,10,30,25,40)
vec_num3 = c(1:9)

vec_num4 = c(2,2,2)
vec_num5 = c(2,4,6)

vec_num1
vec_num2
vec_num3

#Vector Arithmetic
vec_num1+vec_num2
vec_num1-vec_num2
vec_num1*vec_num2
vec_num1/vec_num2
vec_num5^vec_num4

#Matrix
# 2 Dimension Arrays
z = c(1:6)
u = c(4,5,6,7,8,1)
mat_A = matrix(data=z, nrow = 3, ncol= 2, byrow = "False")

mat_A
mat_B = matrix(data=u, nrow = 3, ncol= 2, byrow = "True")

mat_B

#Addition
add_AB = mat_A+mat_B
add_AB

#Subtraction
sub_AB = mat_A-mat_B
sub_AB

#Division
dev_AB = mat_A/mat_B
dev_AB 

#Multiplication
mul_AB = mat_A*mat_B
mul_AB 

#Data Frames
a = c(10,20,30,40)
b = c(1,4)
c = c('M','I','S','O')

df_MY = data.frame(a,b,c)
names(df_MY) = c('ID','VALUES','NAMES')
df_MY

#PIE Chart
x=c(25,45,52,90)
y=c("A","B","C","D")

pie(x,y)

#advance Pie
pie(x,y, main = "title of the pie chart", col = rainbow(4))

#percentage cal
r = round(x/sum(x)*100)
r

new_label =paste(r,"%")
new_label

pie(x,new_label, main = "title of the pie chart", col = rainbow(4))

#Bar_Charts
bar = c(25,34,76,90)

barplot(bar)

barplot(bar, mai="Title of the bar chart")

barplot(bar, mai="Title of the bar chart",col ='#16CE84')

barplot(bar, mai="Title of the bar chart",col ='#16CE84',
        border ='#CE169E', names.arg = c("A","B","C","D"),
        xlab = "Name of the bar chart", ylab = "Y axix name of the bar chart")

barplot(bar, mai="Title of the bar chart",col ='#16CE84',
        border ='#CE169E', names.arg = c("A","B","C","D"),
        xlab = "Name of the bar chart", ylab = "Y axix name of the bar chart",
        horiz =TRUE )
                                         
        
#Read_dataset
getwd()

Student_data = read.csv('stu.csv',header = TRUE, sep=",")
Student_data

#Central Tendency
#measyres of location
# 1) Mean (add all values and divide by how many values there are)
#ex 6,7,8
#Mean = (6+7+8)/3 = 7
mean(Student_data$Marks)

# 2) Median (put numbers in order and pick the middle one)
median(Student_data$Marks)

# 3) Mode(Most repeated value)
#Find the mode
mode_function <-function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

mode_function(Student_data$Marks)

#measures of Dispersion(Range,Variance,standard Deviation,
#IQR,Box plots)
#means how spread out the data is (Measure data variable)
getwd()

Student_data = read.csv('stu.csv',header = TRUE, sep=",")
Student_data

#Calculate Range
range_marks <- max(Student_data$Marks) - min(Student_data$Marks)
range_marks

#Calculate IQR Interquartile Range
iqr_marks <- IQR(Student_data$Marks)
iqr_marks

#variance and standard Deviation
var_marks <- var(Student_data$Marks)
var_marks

sd_marks <- sd(Student_data$Marks)
sd_marks

#Box plot
boxplot(Student_data$Marks, main="Boxplot od Student marks",
        ylab = "Marks",
        col = "orange",
        border = "brown",
        notch = TRUE)

# Add mean line
abline(h = mean(Student_data$Marks),
       col = "red",
       lwd = 2,
       lty = 2)

# Add text lable for mean
text(
  x = 1.3,
  y = mean(Student_data$Marks),
  labels = paste("Mean =", round(mean(Student_data$Marks), 2)),
  col = "red"
)

# Get the dataset path
getwd()

Student_data = read.csv('stu.csv', header = TRUE, sep=",")
Student_data

# Anova Test
result <- aov(Marks~ Gender, data = Student_data)
summary(result)

#           Df Sum Sq Mean Sq F value Pr(>F)
#Gender       1    144   144.1   0.429  0.514
#Residuals   98  32936   336.1               

#if Pr(>F) < 0.05 ==> Statically Significant
#the average marks between Males and Females are Significantly different.

#if Pr (>5) >- 0.05 =→> Not Statically Significant
#There is no strong evidence to say marks are different based on gender

#Frequency Distribution and Interval and midpoint
#1 create class intervals
breaks <- seq(0, 100, by =10) #creates ex :- 0-10, 10-20

#2 cut marks into Intervals
Student_data$Interval <- cut(Student_data$Marks, breaks, right= FALSE)

#3 Frequency table
freq_table <- table(Student_data$Interval)
freq_table

#Midpoint
midpoints <- (head(breaks, -1) + tail(breaks, -1))/2
midpoints

#Create a Midpoint
barplot(freq_table,
        main = "Histigram of Marks",
        xlab = "Marks Range",
        ylab = "Number of students",
        col = "lightblue",
        border = "black")

#Frequency polygon (Midpoint & Lines)
#midpoints
midpoints <- (head(breaks, -1) + tail(breaks, -1))/2

#convert frequency table to numaric vectors
freq_values <- as.numeric(freq_table)

#plot frequency polygen
plot(midpoints, freq_values, type = "o",
     main = "Frequency polygon",
     xlab = "Midpoint of marks range",
     ylab = "Number of students",
     col = "blue",
     pch = 16)

# Bell Curve

# 1 Load data
Student_data <- read.csv("stu.csv", header = TRUE, sep = ",")

# 2 Calculate Mean and Standard Deviation
mean_marks <- mean(Student_data$Marks)
sd_marks <- sd(Student_data$Marks)

# 3 Create Histogram using Density Scale
hist(Student_data$Marks,
     probability = TRUE,
     main = "Histogram with Bell Curve",
     xlab = "Marks",
     ylab = "Density",
     col = "lightblue",
     border = "black")

# 4 Create a sequence of X values
x_values <- seq(min(Student_data$Marks),
                max(Student_data$Marks),
                length = 100)

# 5 Calculate the normal distribution Y values
y_values <- dnorm(x_values,
                  mean = mean_marks,
                  sd = sd_marks)

# 6 Plot the Bell Curve
plot(x_values, y_values,
     type = "l",
     lwd = 2,
     col = "red",
     main = "Bell Curve of Student Marks",
     xlab = "Marks",
     ylab = "Density")

#with package
install.packages("ggplot2")
library(ggplot2)





# 7 Display Mean and Standard Deviation
mean_marks
sd_marks

# 4 Add Bell Curve
curve(dnorm(x,
            mean = mean_marks,
            sd = sd_marks),
      col = "red",
      lwd = 3,
      add = TRUE)

# 5 Add Mean Line
abline(v = mean_marks,
       col = "darkgreen",
       lwd = 2,
       lty = 2)

# 6 Add Legend
legend("topright",
       legend = c("Bell Curve", "Mean"),
       col = c("red", "darkgreen"),
       lwd = c(3, 2),
       lty = c(1, 2))


