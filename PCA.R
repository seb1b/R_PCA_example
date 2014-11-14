#Sebastian Bittel


#short script doing a PCA on a 2 dim data set, data set is given by a 
#gaussian desitribution with the following mean an d cov matrix
mu = c(1,2)
sigma = matrix(c(2.0,1.3, 1.3, 1.0),2,2)
n = 1000


gauss = mvrnorm(n, mu, sigma)
plot(gauss[,1], gauss[,2], xlab = "x",ylab = "y")

#calculate eigenvecotr matrix
center_of_mass = 1/n * sum(gauss)
gauss_normalized = gauss - center_of_mass
plot(gauss_normalized[,1], gauss_normalized[,2], xlab = "x",ylab = "y")
gauss_normalized_t = t(gauss_normalized)
cov = 1/n * (gauss_normalized_t %*% gauss_normalized)
eigen_ = eigen(cov)
eigenvalues = eigen_$values
data_projected = gauss_normalized %*% eigen_$vectors
plot(data_projected[,1], data_projected[,2], xlab = "x",ylab = "y")

#choose more significant PC
if(eigenvalues[1] > eigenvalues[2]){
  red_data_projected = data_projected[,1]
  red_gauss = gauss_normalized[,1]
}else {
  red_data_projected = data_projected[,2]
  red_gauss = gauss_normalized[,2]
  
}

#calculate mean squarred error
#for the transformed data
trans_mu_ = 1/n * sum(red_data_projected)
trans_var_ = 1/n * sum((red_data_projected-trans_mu_)*(red_data_projected-trans_mu_))
trans_MSE = trans_var_/n

#for the reduced original data
orig_mu_ = 1/n * sum(red_gauss)
orig_var_ = 1/n * sum((red_gauss-orig_mu_)*(red_gauss-orig_mu_))
orig_MSE = orig_var_/n