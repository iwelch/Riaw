## must be started with R --vanilla; not elegant or efficient, but easy to understand

all.sources <- search()
d <- NULL
for (i in 1:length(all.sources)) {
  all.functions <- ls(search()[i])
  N <- length(all.functions)
  if (N==0) next
  d <- rbind(d, data.frame( src=rep(all.sources[i], N), index=1:N, fname=all.functions ) )
}
