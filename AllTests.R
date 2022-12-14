#' Sore points
#' * Accuracy: 
#' ---> is-a-means-to issue.
#' * Deficit: great!
#' * Excess:
#' ---> Total Excess, very small margin. Solution: just remove it? It is a small margin..
#' * Overlap:
#' ---> 

expAlphaLevel = 0.05/4 # One piece for each AC, DEF, EXC, OV
accExps = 6
defExps = 2
excExps = 2
ovExps = 2

#Test 1
cat(reportWilcox(pre_Language,alpha = expAlphaLevel/(accExps*2)))
cat(reportWilcox(rec_Language,alpha = expAlphaLevel/(accExps*2)))

#Test 2
  cat(reportWilcox(acc.pp.a3.result,alpha = expAlphaLevel/(accExps)))

#Test 3
cat(reportWilcox(acc.pp.a4.pre.result,alpha = expAlphaLevel/(accExps*2)))
cat(reportWilcox(acc.pp.a4.rec.result,alpha = expAlphaLevel/(accExps*2)))

#Test 4
cat(reportWilcox(acc.pp.a5.pre.result,alpha = expAlphaLevel/(accExps*2)))
  cat(reportWilcox(acc.pp.a5.rec.result,alpha = expAlphaLevel/(accExps*2)))

#Test 5
#  cat(reportWilcox(acc.pp.a6.pre.result,alpha = expAlphaLevel/(accExps*2)))
cat(reportWilcox(acc.pp.a6.rec.result,alpha = expAlphaLevel/(accExps*2)))

#Test 6
#  cat(reportWilcox(acc.pp.a7.pre.result,alpha = expAlphaLevel/(accExps*2)))
#cat(reportWilcox(acc.pp.a7.rec.result,alpha = expAlphaLevel/(accExps*2)))


#Test 7 - DEFICIT
cat(reportWilcox(def.ent.res,alpha = expAlphaLevel/(defExps)))

#Test 8 - DEFICIT
cat(reportWilcox(def.rel.res,alpha = expAlphaLevel/(defExps)))

#Test 8 - REMOVE
cat(reportWilcox(exc.res,alpha = expAlphaLevel/excExps))

#Test 8
ifelse(exs.wilx_ <= expAlphaLevel/(defExps/7), paste0(round(exs.wilx_,3),"*"),round(exs.wilx_,3))

#Test 9
cat(reportWilcox(OVP.ent.res,alpha = expAlphaLevel/(ovExps)))

#Test 10
cat(reportWilcox(OVP.rel.res,alpha = expAlphaLevel/(ovExps)))

