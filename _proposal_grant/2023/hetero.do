program drop _all
program define hetero
version 12.1
args rrr1 lcl1 ucl1 rrr2 lcl2 ucl2 rrr3 lcl3 ucl3 rrr4 lcl4 ucl4 rrr5 lcl5 ucl5
 
*first RRR
disp as text "First RRR (95% CI): " as result %4.2f `rrr1' " (" ///
       %4.2f `lcl1' ", " %4.2f `ucl1' ")"
 
*second RRR
disp as text "Second RRR (95% CI): " as result %4.2f `rrr2' " (" ///
       %4.2f `lcl3' ", " %4.2f `ucl3' ")"
		
*third RRR
disp as text "Third RRR (95% CI): " as result %4.2f `rrr3' " (" ///
       %4.2f `lcl3' ", " %4.2f `ucl3' ")"
	   
*fourth RRR
disp as text "Fourth RRR (95% CI): " as result %4.2f `rrr4' " (" ///
       %4.2f `lcl4' ", " %4.2f `ucl4' ")"

*fifth RRR
disp as text "Fifth RRR (95% CI): " as result %4.2f `rrr5' " (" ///
       %4.2f `lcl5' ", " %4.2f `ucl5' ")"	   
	   
*variances
local var1 = ((ln(`ucl1')-ln(`rrr1'))/invnormal(0.975))^2
local var2 = ((ln(`ucl2')-ln(`rrr2'))/invnormal(0.975))^2
local var3 = ((ln(`ucl3')-ln(`rrr3'))/invnormal(0.975))^2
local var4 = ((ln(`ucl4')-ln(`rrr4'))/invnormal(0.975))^2
local var5 = ((ln(`ucl5')-ln(`rrr5'))/invnormal(0.975))^2
 
* pooled estimate
local num_pool = ((ln(`rrr1')/`var1') + (ln(`rrr2')/`var2') + (ln(`rrr3')/`var3') + (ln(`rrr4')/`var4') + (ln(`rrr5')/`var5'))
local denom_pool = ((1/`var1') + (1/`var2') + (1/`var3') + (1/`var4') + (1/`var5'))
local pooled = `num_pool' / `denom_pool'
disp as text "Pooled RRR= ", as result %4.2f exp(`pooled')
 
* cochran's Q
local q = (((ln(`rrr1')-`pooled')^2)/`var1') + (((ln(`rrr2')-`pooled')^2)/`var2') + (((ln(`rrr3')-`pooled')^2)/`var3') + (((ln(`rrr4')-`pooled')^2)/`var4') + (((ln(`rrr5')-`pooled')^2)/`var5')
disp as text "Cochran's Q = ", as result %4.2f `q' as text "  p-value = ", ///
       as result %5.4f chi2tail(1, `q')
 
* altman
disp " "
disp " "
disp as text "Altman test for interaction"
disp " "
local drrr = ln(`rrr1') - ln(`rrr2')
disp as text "Diff in log RRRs: ", as result %5.3f `drrr'
 
local serrr1 = (ln(`lcl1') - ln(`ucl1')) / (2*invnormal(0.975))
local serrr2 = (ln(`lcl2') - ln(`ucl2')) / (2*invnormal(0.975))
local se_drrr = sqrt((`serrr1'*`serrr1' + `serrr2'*`serrr2'))
 
disp as text "95% CI for diff: " as result "(" %4.3f `drrr'-(invnormal(0.975)*`se_drrr') ", " ///
       %4.3f `drrr'+(invnormal(0.975)*`se_drrr') ")"
disp as text "Test of interaction: " as result "z = " %5.3f `drrr'/`se_drrr' ///
       "  p = " %5.4f       2*(1-(normal(abs(`drrr'/`se_drrr')))) 
disp as text "Ratio of estimates, 95% CI: " as result %4.2f exp(`drrr')  " (" ///
       %4.2f exp(`drrr'-invnormal(0.975)*`se_drrr') ", " %4.2f exp(`drrr'+invnormal(0.975)*`se_drrr') ")"
end

/*
. hetero 1.02 1.00 1.05 1.04 1.01 1.07 0.97 0.92 1.02 0.99 0.92 1.05 1.14 1.06 1.23
First RRR (95% CI): 1.02 (1.00, 1.05)
Second RRR (95% CI): 1.04 (0.92, 1.02)
Third RRR (95% CI): 0.97 (0.92, 1.02)
Fourth RRR (95% CI): 0.99 (0.92, 1.05)
Fifth RRR (95% CI): 1.14 (1.06, 1.23)
Pooled RRR=  1.02
Cochran's Q =  14.60  p-value =  0.0001
 
*/