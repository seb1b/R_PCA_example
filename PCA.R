#Sebastian Bittel


#short script doing a PCA on a 2 dim data set, data set is given by a 
#gaussian desitribution with the following mean an d cov matrix
library("MASS")

mu = c(1,2)
sigma = matrix(c(2.0,1.3, 1.3, 1.0),2,2)
n = 1000


data = mvrnorm(n, mu, sigma)
plot(gauss[,1], gauss[,2],main = "data", xlab = "x",ylab = "y")

#calculate eigenvecotr matrix
cov = cov(data)
print(cov)
eigen_ = eigen(cov)
eigenvalues = eigen_$values

#transform data
Q = eigen_$vectors * -1
print(Q)
trans_data = gauss %*% Q
plot(trans_data[,1], trans_data[,2],main = "transformend Data", xlab = "x",ylab = "y")

red_trans_data==0
red_data=0
#choose more significant PC
if(eigenvalues[1] > eigenvalues[2]){
  red_trans_data= trans_data[,1]
  red_data = data[,1]
  red_trans_data <- cbind(red_trans_data, mean(trans_data[,2]))
  red_data <- cbind(red_data, mean(data[,2]))
}else {
  red_trans_data = trans_data[,2]
  red_gauss = data[,2]
  red_trans_data <- cbind(red_trans_data, mean(trans_data[,1]))
  red_data <- cbind(red_data, mean(data[,1]))
  
}

plot(red_trans_data[,1], red_trans_data[,2],main = "reduced transformend Data")
plot(red_data[,1], red_data[,2],main = "reduced Data")

#calculate mean squarred error
#for the transformed data
diff_red_data = trans_data - red_trans_data
trans_MSE  = mean(diff_red_data^2)
print(trans_MSE)
#for the original data
diff_data = data - red_data
orig_MSE  = mean(diff_data^2)
print(orig_MSE)