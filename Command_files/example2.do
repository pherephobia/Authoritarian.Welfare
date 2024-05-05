set seed 2
matrix Sigma = J(2, 2, 0.5) + 0.5*I(2)
matrix gof_pvaluesum_mvn = J(4, 5, 0)
matrix gof_pvaluesum_mix = J(4, 5, 0)
local i = 1
foreach n of numlist 10 25 50 100 {
  local j = 1
  foreach df of numlist 2 5 10 50 100 {
    foreach rep of numlist 1/1000 {
	  rmvt, delta(0, 0) sigma(Sigma) df(`df') n(`n') method(chol)
	  matrix randsamp = r(rmvt)
	  quietly svmat randsamp
	  quietly mvtest normality randsamp1 randsamp2
	  matrix gof_pvaluesum_mvn[`i', `j'] = gof_pvaluesum_mvn[`i', `j'] + r(p_dh)
	  drop randsamp1 randsamp2
	  rtmvt, delta(-2, -2) sigma(Sigma) df(`df') n(`n') method(chol) lowertruncation(-2.5, -2.5) uppertruncation(-1.5, -1.5)
	  matrix randsamp = r(rtmvt)
	  rtmvt, delta(2, 2) sigma(Sigma) df(`df') n(`n') method(chol) lowertruncation(1.5, 1.5) uppertruncation(2.5, 2.5)
	  matrix randsamp = (randsamp \ r(rtmvt))
	  quietly svmat randsamp
	  quietly mvtest normality randsamp1 randsamp2
	  matrix gof_pvaluesum_mix[`i', `j'] = gof_pvaluesum_mix[`i', `j'] + r(p_dh)
	  drop randsamp1 randsamp2
	}
    local `j++'
  }
  local `i++'
}
matrix gof_summary_mvn = gof_pvaluesum_mvn/1000
matrix gof_summary_mix = gof_pvaluesum_mix/1000
