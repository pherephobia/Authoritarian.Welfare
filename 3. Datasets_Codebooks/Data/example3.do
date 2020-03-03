set seed 3
matrix Sigma   = J(3, 3, 0.5) + 0.5*I(3)
invmvnormal, p(0.95) mean(0, 0, 0) sigma(Sigma) tail("lower")
local r = r(quantile)
pmvnormal, lower(., ., .) upper(`r', `r', `r') mean(0, 0, 0) sigma(Sigma)
pmvnormal, lower(., ., .) upper(`= invnormal(1 - 0.05/3)', `= invnormal(1 - 0.05/3)', `= invnormal(1 - 0.05/3)') mean(0, 0, 0) sigma(Sigma)
