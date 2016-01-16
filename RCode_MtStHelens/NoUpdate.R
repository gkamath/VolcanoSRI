NoUpdate <- function(Genssys,No_of_col)
{
  rr = length(Genssys)
  sindex = rep(0,No_of_col)
  for(i in 1:rr)
  {
    index = Genssys[[i]]$idx
    for(j in 1:length(index))
    {
      sindex[index[j]] = 1
    }   
  }
  return(sindex)
}