set seed 4
matrix Sigma = J(3, 3, 0.5) + 0.5*I(3)
matrix list Sigma
pmvnormal, lower(0, 0, 0) upper(., ., .) mean(0, 0, 0) sigma(Sigma)
matrix lowertrunc    = J(110, 1, 0)
matrix probabilities = J(110, 1, 0)
foreach i of numlist 1/110 {
  matrix lowertrunc[`i', 1] = -10 + 0.1*(`i' - 1)
  quietly tmvnormal, lower(0, 0, 0) upper(., ., .) mean(0, 0, 0) sigma(Sigma) lowertruncation(`=lowertrunc[`i', 1]', `=lowertrunc[`i', 1]', `=lowertrunc[`i', 1]') uppertruncation(., ., .)
  matrix probabilities[`i', 1] = r(integral)
}
matrix data = (lowertrunc, probabilities)
quietly svmat data
twoway (line data2 data1, yaxis(1)), xtitle(t) ytitle(P(X >= (0,0,0)|(t,t,t))) scheme(sj)
