#################################################
# get a data set


#' Get Data Set from the R global environment
#'
#' @param data.set the name of the data set
#'
#' @return returns the data set as a data frame from the R global environment
#' @export
#'
#' @examples
#'#error_in_code
#'
get.data.set=function(data.set)
{
  temp=deparse(match.call())
  dset.name=get.arg(temp,"data.set")
  ls.res0=ls(topenv())
  ls.res=ls.res

  mtch=grep(dset.name,ls.res)
  if (length(mtch)==1)
  {
    res=eval(parse(text=dset.name))
    res=data.frame(res)
    return(res)
  }

  print(dset.name)
  mtch=agrep(dset.name,ls.res)
  if (length(mtch)==1)
  {
    print(ls.res0[mtch])
    res=eval(parse(text=ls.res0[mtch]))
    res=data.frame(res)
    return(res)
  }

  stop(paste0("Unable to find data.set ",data.set,".  "))

}


###########################################
# Extract a column of data from a data set

#' Get Column Data
#'
#' @param y the exact column name or close match to its name
#' @param dset the data set from which the column data will be extracted
#'
#' @return returns the 'y' column data from the data set
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' get.y.clm("Supp",data_frame)
#'
get.y.clm=function(y,dset)

{
  y.clm=arg.as.char(y)
  dset.clms=colnames(dset)
  clms=colnames(dset)


  mtch=which(is.element(clms,y.clm))
  if (length(mtch)==1)
  {
    y=dset[,mtch]
    attr(y,"clm.name")=dset.clms[mtch]
    return(y)
  }

  clms=tolower(clms)
  if (!any(duplicated(clms)))
  {
    y.clm=tolower(y.clm)
    mtch=which(is.element(clms,y.clm))
    if (length(mtch)==1)
    {
      y=dset[,mtch]
      attr(y,"clm.name")=dset.clms[mtch]
      return(y)
    }

    mtch=agrep(y.clm,clms)
    if (length(mtch)==1)
    {
      y=dset[,mtch]
      attr(y,"clm.name")=dset.clms[mtch]
      return(y)
    }
  }


  stop(paste0("Column ",y.clm," not found in dset."))

}

#################################
# Represent input argument as a character

#' Represent input argument as a character
#'
#' @param x the input argument
#'
#' @return returns the input argument as character string
#' @export
#'
#' @examples
#' arg.as.char(binarydata)
#'
arg.as.char=function(x)
{
  temp=try(x,silent=T)
  #if (class(temp)=="character") #TODO: delete if below code works
  if( is(temp,"character"))
    return(temp)

  temp=deparse(match.call())
  res=get.arg(temp,"x")
  return(res)
}

#######################################
# get the argument value


#' Get Argument Value
#'
#' @param call.string the string that is called and converted into string
#' @param arg.string the string to match with
#'
#' @return #ask_Stan
#' @export
#'
#' @examples
#' #ask_Stan
#'
get.arg=function(call.string, # obtain with deparse(match.call())
                 arg.string)  # character string with argument name
{

  x.pos=regexpr(paste0(arg.string," = "),call.string,fixed=T)
  x.name=substring(call.string,x.pos+nchar(arg.string)+3)
  x.name=x.name[x.pos>0]


  comma.pos=regexpr(",",x.name,fixed=T)
  close.pos=regexpr(")",x.name,fixed=T)
  end.pos=close.pos

  if (length(comma.pos)>0)
  {
    if (comma.pos>0) end.pos=comma.pos
  }

  x.name=substring(x.name,1,end.pos-1)


  x.name=gsub('\"','',x.name)

  return(x.name)
}


###################################
# Normality test
# perform shapiro.test if n < 3000
# otherwise perform stats::ks.test

#' Perform a Normality Test
#'
#' @param x the feature in the data set whose distribution will be tested.
#'
#' @return returns the p-value of the feature that implies the distribution of the data.
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' normality.test(data_frame$len)
#'
normality.test=function(x)
{
  na=is.na(x)
  n=length(x)

  if (n<3000)
  {
    res=stats::shapiro.test(x)
    return(res)
  }

  res=stats::ks.test(x,"pnorm")
  return(res)
}


#################################
# fresh start
# clean out R memory and start over

#' Fresh R Session
#'
#' @return clears the R environment and starts new session with the source files and packages.
#' @export
#'
#' @examples
#' fresh.start()
#'
fresh.start=function()
{
  rm(list=ls())
  SBP.code.path="https://raw.githubusercontent.com/stan-pounds/Simple-Biostats-Program/main/"
  source(paste0(SBP.code.path,"setup-SBP.R"))
}
