matrix Sigma        = J(2, 2, 0.5) + 0.5*I(2)
matrix mvnormalden  = J(101, 101, 0)
matrix mvtden       = J(101, 101, 0)
matrix tmvnormalden = J(101, 101, 0)
matrix tmvtden      = J(101, 101, 0)
local i = 1
foreach x1 of numlist -3(0.06)3 {
  local j = 1
  foreach x2 of numlist -3(0.06)3 {
    quietly mvnormalden, x(`x1', `x2') mean(0, 0) sigma(Sigma)
    matrix mvnormalden[`i', `j'] = r(density)
    quietly mvtden, x(`x1', `x2') delta(0, 0) sigma(Sigma) df(1)
    matrix mvtden[`i', `j'] = r(density)
    quietly tmvnormalden, x(`x1', `x2') mean(0, 0) sigma(Sigma) lowertruncation(-1.5, -1.5) uppertruncation(1.5, 1.5)
    matrix tmvnormalden[`i', `j'] = r(density)
    quietly tmvtden, x(`x1', `x2') delta(0, 0) sigma(Sigma) lowertruncation(-1.5, -1.5) uppertruncation(1.5, 1.5) df(1)
    matrix tmvtden[`i', `j'] = r(density)
    local `j++'
  }
  local `i++'
}
quietly svmat mvnormalden
quietly svmat mvtden
quietly svmat tmvnormalden
quietly svmat tmvtden
generate row = _n
quietly reshape long mvnormalden mvtden tmvnormalden tmvtden, i(row) j(col)
local opt " aspect(2) ccuts(0 0.03 0.06 0.09 0.12 0.15 0.18 0.21) xtitle("x{subscript:1}") ytitle("x{subscript:2}")"
twoway contour mvnormalden row col, nodraw saving(g1,replace) `opt'
twoway contour mvtden row col, nodraw saving(g2,replace) `opt'
twoway contour tmvnormalden row col, nodraw saving(g3,replace) `opt'
twoway contour tmvtden row col, nodraw saving(g4,replace) `opt'
graph combine g1.gph g2.gph g3.gph g4.gph, common imargin(0 0 0 0) rows(2) cols(2)
