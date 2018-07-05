#' Descriptive statistics for continuous data.

#' @param interval A numeric vector
#' @param freq A numeric vector

#' @return Descriptive statistics (number of observations, mean, median, variance, standard deviation, X^2 value, X^2 check (p-value)) of given data 
#' @examples 
#' stats.d.c(interval, frequency)
#' stats.d.c(c(0,10,20,30),c(5,25,2))

#' @author Zisis Dimitrios, student of Dept. Applied Informatics, University of Macedonia

stats.d.c=function(interval,freq)
{
  
  ## We store the actual values of the two extremes (for the barplot appearance)
  org_start = interval[1]
  org_end = interval[length(interval)]
  
  ## Number of classes
  classes = length(interval)-1
  
  ## Central Values
  values = c()
  k=1
  while(k<length(interval)){
    values[k] = (interval[k]+interval[k+1])/2
    k=k+1
  }
  values <- values[!is.na(values)]
  
  k=2
  max=abs(interval[1]-interval[2])
  names.arg = c("")
  names.arg[1] = paste(as.character(interval[1]), " - ", (as.character(interval[2])))
  while(k<length(interval)){
    if (abs(interval[k]-interval[k+1])>max){
      max = abs(interval[k]-interval[k+1])
      max.index = k
    }
    ## In names.arg we store the labels we put in the barplot for each class
    names.arg[k] = paste(as.character(interval[k]), " - ", (as.character(interval[k+1])))
    k=k+1
  }
  
  ## Remove any NA values from the alphanumeric names.arg vector
  names.arg <- names.arg[!is.na(names.arg)]
  
  ## Find the max of freq
  max.freq = max(freq)
  
  ## Calculate a vector of related frequencies
  relative.freq = freq/max.freq

  ## Make label text perpendicular to axis
  par(las = 2)
  
  ## Increase y-axis margin.
  par(mar = c(5,8,4,2)) 
  ## Barplot construction (The ylim parameter can be modified depending on the size of the frequencies)
  barplot(relative.freq, space=0.5, main="Barplot", horiz = F, names.arg=names.arg, col="brown", cex.names=0.8, ylim = c(0, max(relative.freq)))
  
  ## No warnings
  options(warn = -1) 
  
  ## Check if data set name has been missed
  if(missing(values) || missing(freq)) stop("CAUTION! You did not specify a vector of data")
  
  ## Check the data set to be numeric
  if(!is.numeric(values) || !is.numeric(freq)) stop("CAUTION! Data is not a numeric vector. Try as.numeric ()")	
  
  ## Check the sample size  
  n = sum(na.omit(freq));
  n0 = sum(freq)
  if(n < 2) stop("CAUTION! Less than two valid comments")
  
  aux=paste("      DESCRIPTIVE STATISTICS ");names(aux) = c("");print(aux,quote=F)
  
  ## Calculate mean
  freq = na.omit(freq)
  n = sum(freq)
  my.mean = sum(values*freq)/n
  
  ## Calculate variance
  my.var = sum((values-my.mean)^2*freq)/(sum(freq)-1)
  
  ## Calculate standard deviation
  my.sd = sqrt(my.var)
  
  ## Calculate median
  ## i is the group containing what would be the median
  i = match(max(freq),freq) 
  ## Fi is the number of values (frequency) in group i
  Fi = max(freq) 
  if (i>1){
    ## Li is the lower end of the range in group i
    Li = interval[i-1] 
    ## Ri is the range of values in group i
    Ri = interval[i]-interval[i-1]
  }
  else{
    ## Li is the lower end of the range in group i
    Li = interval[i]
    ## Ri is the range of values in group i
    Ri = interval[i]-interval[i-1]
  }
  ## CFi is the total number of values (cumulative frequency) of all groups before group i
  CFi = 0
  j = 1
  while (freq[j] != max(freq)){
    CFi = CFi + freq[j]
    j = j+1
  }
  my.median = Li+(((n+1)/2)-CFi)/Fi*Ri
  
  ## X^2 Check
  # Theoretical probabilities from normal with μ = my.mean, σ = my.sd and expected frequencies
  # Preliminary calculation
  upper.bounds = c(interval[1]:interval[length(interval)])
  theoretical.prob.aux = c(pnorm(upper.bounds,mean=my.mean,sd=my.sd,lower.tail=TRUE))
  theoretical.prob = c(theoretical.prob.aux[1],theoretical.prob.aux[2:classes]-theoretical.prob.aux[1:classes-1],1-theoretical.prob.aux[classes]) 
  ExpFreq = n*theoretical.prob
  ObsFreq = freq
  Res = ObsFreq-ExpFreq
  chi.square = sum((Res)^2/ExpFreq)
  
  # Final calculation
  ObsFreq.final = c(sum(ObsFreq[1:2]),ObsFreq[3:classes])
  ExpFreq.final = c(sum(ExpFreq[1:2]),ExpFreq[3:classes])
  
  ## Degrees of freedom df = n-2-1 and calculation of p-value
  df=classes-2-1
  p.value=pchisq(chi.square,df,lower.tail=FALSE)
  
  ## Results
  name = c("number of observations", "mean", "median", "variance","standard deviation", "X^2 value", "X^2 check (p-value)")
  value = c(n0,my.mean, my.median, my.var, my.sd, chi.square, p.value)
  d.names.1 = rep("",length(value))
  d.names.2 = c("     NAME","  VALUE")
  d.names = list(d.names.1,d.names.2)
  aux = cbind(name,value)
  dimnames(aux) = d.names
  
  print(aux,na.print="",quote=FALSE)
  
}