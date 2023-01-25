% usefile C:/Users/R2/Documents/rebeca/resultados/dir_46_tau_0.1//bayesx.estim.input.prg

logopen using C:/Users/R2/Documents/rebeca/resultados/dir_46_tau_0.1//bayesx.estim.input.prg.log

bayesreg b

dataset d 
d.infile using C:/Users/R2/Documents/rebeca/resultados/dir_46_tau_0.1//bayesx.estim.data.raw

b.outfile = C:/Users/R2/Documents/rebeca/resultados/dir_46_tau_0.1//bayesx.estim

b.regress y = idade + directionx1 + directionx2, family=quantreg iterations=2000 burnin=1000 step=1 setseed=1739287037 quantile=0.1 predict using d 

b.getsample

logclose 
