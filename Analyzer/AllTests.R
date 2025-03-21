#' Sore points
#' * Accuracy: 
#' ---> is-a-means-to issue.
#' * Deficit: great!
#' * Excess:
#' ---> Total Excess, very small margin. Solution: just remove it? It is a small margin..
#' * Overlap:
#' ---> 

accExps = 10

#Test 1
cat(reportWilcox(pre_Language,alpha = expAlphaLevel/(accExps)))
cat(reportWilcox(rec_Language,alpha = expAlphaLevel/(accExps)))

#Test 2
  cat(reportWilcox(acc.pp.a3.result,alpha = expAlphaLevel/(accExps)))

#Test 3
cat(reportWilcox(acc.pp.a4.pre.result,alpha = expAlphaLevel/(accExps)))
cat(reportWilcox(acc.pp.a4.rec.result,alpha = expAlphaLevel/(accExps)))

#Test 4
cat(reportWilcox(acc.pp.a5.pre.result,alpha = expAlphaLevel/(accExps)))
  cat(reportWilcox(acc.pp.a5.rec.result,alpha = expAlphaLevel/(accExps)))

#Test 5
#  cat(reportWilcox(acc.pp.a6.pre.result,alpha = expAlphaLevel/(accExps*2)))
cat(reportWilcox(acc.pp.a6.rec.result,alpha = expAlphaLevel/(accExps)))

#Test 6
cat(reportWilcox(acc.pp.a7.pre.result,alpha = expAlphaLevel/(accExps)))
cat(reportWilcox(acc.pp.a7.rec.result,alpha = expAlphaLevel/(accExps)))



#Test 7 - DEFICIT
cat(reportWilcox(def.ent.res,alpha = expAlphaLevel/(defExps)))

#Test 8 - DEFICIT
cat(reportWilcox(def.rel.res,alpha = expAlphaLevel/(defExps)))

#Test 8 - REMOVE
  cat(reportWilcox(exc.res,alpha = expAlphaLevel/excExps))

#Test 8
ifelse(exs.wilx_ <= expAlphaLevel/(excExps*7), paste0(round(exs.wilx_,3),"*"),round(exs.wilx_,3))

#Test 9
cat(reportWilcox(exc.prevents,alpha = expAlphaLevel/excExps))

#Test 9
cat(reportWilcox(OVP.ent.res,alpha = expAlphaLevel/(ovExps)))

#Test 10
cat(reportWilcox(OVP.rel.res,alpha = expAlphaLevel/(ovExps)))


#Test 11 - SELF
cat(reportKendalCor(def.self.corr,alpha = expAlphaLevel/(selfExps)))

#Test 12 - SELF
cat(reportKendalCor(excessCorr.all.corr,alpha = expAlphaLevel/(selfExps)))

#Test 13 - SELF
cat(reportKendalCor(excessCorr.all.corr,alpha = expAlphaLevel/(selfExps)))

#Test 14 - SELF
cat(reportKendalCor(ov.corr.test,alpha = expAlphaLevel/selfExps))





