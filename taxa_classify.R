
#### define the function #####

label_abund_class = function(dt,abund.cut,freq.cut,rare.cut){
  abund_pos = c(dt[['abundant.count']]>=abund.cut & dt[['freq']]>=freq.cut)
  rare_pos = c(dt[['rare.count']]==rare.cut)
  dt[['class_abund']][abund_pos] = 'Abund.'
  dt[['class_abund']][rare_pos] = 'Rare'
  dt[['class_abund']][!abund_pos & !rare_pos]='CRT'
  dt[['class_abund']] = as.factor(dt[['class_abund']])
  dt[['class_abund']]
}

### classify rare and abundant

cutoff=0.001
comm.RA=ifelse(comm.percent>=cutoff,1,0)
RA.count<-data.frame(abundant.count = colSums(comm.RA),
                     rare.count = nrow(comm.RA) - colSums(comm.RA))
RA.sum<-data.frame(RA.count,
                   otu.sum.rank[match(rownames(RA.count),rownames(otu.sum.rank)),],
                   copynumber=cpn.otu[match(rownames(RA.count),rownames(cpn.otu)),])




#### define abundant ######

abund.cut=nrow(comm.use)*0.5
freq.cut=nrow(comm.use)*0.8
rare.cut=nrow(comm.use)

RA.sum$class_abund =label_abund_class(RA.sum,abund.cut,freq.cut,rare.cut)
