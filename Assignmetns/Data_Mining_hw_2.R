library(mlbench)
data("Ionosphere")
x = as.matrix(read.csv("/Users/ashish/Downloads/rachmaninov_pc2_onset.csv"))

#Question 1.1
#transpose to plot the data
y = t(x)
matplot(y[2:13,], type = 'b', xlab = "note", ylab = "time in secs", main = "Plot of onset time for each measure")

#Question 1.2
#Covariance matrix of x
S1 = cov(x[,2:13])
#mean centering the data
z = scale(x[,2:13], scale = FALSE)
#property to be proved 
S2 = 1/(nrow(z) - 1) * t(z) %*% z
#rounding off to 4 decimal 
s1 = round(S1, digits = 4)
s2 = round(S2, digits = 4)
#checking both the coavraine matrix
s1 == s2

#Question 1.3
x = x[,-(1)]
#mean vector
x_mean = colMeans(x)
cov_x = cov(x)
svd_x = svd(cov_x)
d_x = svd_x$d
u_x = svd_x$u

points = matrix(0, nrow = 1000, ncol = 12)
for (i in 1:1000){
  z_i = rnorm(12, mean = 0, sd = d_x)
  points [i,] = x_mean + colSums(z_i*u_x)
}
points

points = scale(points, scale = FALSE)
#Question 3.1
A = as.matrix(read.csv("/Users/ashish/Downloads/mystery.csv"))

cov_A = cov(A)

#Quesion 3.2
#singular value decomposition matrix 
svd_A = svd(cov_A)
#D matrix
d_A = svd_A$d
#U matrix
u_A = svd_A$u

#Question 3.3
#Dimensiionality reduction using the formula in notes
x_bar = A %*% t(u_A[1:5,])


#Question 4
ion = as.matrix(Ionosphere)

#removing first two columns
ion1 = ion[,-(1:2)]

#removing last column
ion2 = ion1[,-(ncol(ion1))]

#changing it to numeric to calculate majalanobis for each row
ion2 <- matrix(as.numeric(ion2), ncol = ncol(ion2))

#separating good and bad class matrix
ion1_df = as.data.frame(ion1, header = TRUE)
ion1_df$Class
ion_df_good = subset(ion1_df, Class == "good")
ion_df_bad = subset(ion1_df, Class == "bad")
ion_good = as.matrix(ion_df_good)[,-(ncol(ion_df_good))]
ion_bad = as.matrix(ion_df_bad)[,-(ncol(ion_df_bad))]

#calculating mean and covariance of good and bad class matrix
ion_good <- matrix(as.numeric(ion_good), ncol = ncol(ion_good))
ion_bad <- matrix(as.numeric(ion_bad), ncol = ncol(ion_bad))

#calculating mahalanobis of each row for good and bad class
mahalanobis_good = mahalanobis(ion2, colMeans(ion_good), cov(ion_good))
mahalanobis_bad = mahalanobis(ion2, colMeans(ion_bad), cov(ion_bad))
class_predict = ifelse(abs(mahalanobis_good) < abs(mahalanobis_bad), "good", "bad")

#Creating confusion matrix for predicted and actual value
confusion_matrix <- as.matrix(table(factor(ion1[,"Class"], levels=c("bad", "good")), class_predict))
confusion_matrix