############################################
# Compute descriptive stats, tables, figures, and narrative

#' Describe a Variable/Feature
#'
#' @description
#' The `describe()` function describes a variable/feature of a data set. Given a variable and a data set,
#'  it computes descriptive stats of that variable based on its data type.
#'  * If the variable is of `survival` class, it describes its event timing statistics.
#'  * If the variable is of `numeric` class, it describes its numeric statistics.
#'  * If the variable is of `factor` class, it describes its categorical statistics.
#'
#' The descriptive statistics of the variable is presented as text narrative, plots, tables, references.
#'
#' @param clm.name the name of the column in quotation marks.
#' @param data the name of the data set.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 1.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param clr The color in the plot(s). Default value is NULL.
#' @param y.name name of x in quotation marks for narrative. #ask_Stan
#' @param use.all indicates whether to include all data regardless of missingness. Default value is True. #ask_Stan
#'
#' @return returns statistical description of the variable in terms of tables, figures and narratives.
#' @export
#'
#' @examples
#' data_frame <- data.frame(len = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          supp = c("VC","OJ","VC","VC","VC","OJ","VC","OJ","VC","OJ"),
#'                          dose = c(0.5, 0.5, 0.5, 2.0, 1.5, 1.0, 1.0, 2.0, 0.5, 2.0))
#' describe("len",data_frame)
#'
describe=function(clm.name,    # name of column in quotation marks
                  data,        # data set as a data.frame
                  tbl=1,       # tabular output (0=none; 1=basic; 2=detailed)
                  fig=1,       # figure output (0=none; 1=basic; 2 and higher = more)
                  txt=1,       # narrative output (0=none; 1=basic; 2=detailed)
                  clr=NULL,    # color(s) to use
                  y.name=NULL, # name of x in quotation marks for narrative
                  use.all=T)   # indicates whether to include all data regardless of missingness

{
  try.clm=try(clm.name,silent=T)
  #if (class(try.clm)=="try-error") #TODO: delete if below code works
  if( is(try.clm,"try-error"))
  {
    temp=deparse(match.call())
    clm.name=get.arg(temp,"clm.name")
  }

  data=data.frame(data)
  x=get.y.clm(clm.name,data)
  cls=class(x)

  if (is.null(y.name))
  {
    y.name=attr(x,"clm.name")
  }



  res=NULL
  res.est=NULL
  res.out=NULL
  if(any(cls%in%c("numeric","double","integer")))
    res=describe.numeric(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=y.name)

  if(any(cls%in%c("character","factor","ordered")))
    res=describe.categorical(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=y.name,use.all=use.all)

  if(any(cls%in%c("Surv","competing.events")))
    res=describe.event.timing(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=y.name)

  if (is.null(res))
    stop("Invalid input class.")
  
  res.est = estimate(clm.name, data, null=NULL, tbl, fig, txt, clr, y.name,use.all)

  res.txt=unique(c(res$txt,
                 res.est$txt))
  
  comb.res=list(txt=res.txt, #Multiple output with relevant info
                tbl=res.est$tbl, #Only need one of them or it double lists
                method=c(res$method,res.est$method), #res would double print that Shapiro Wilks was used
                ref=unique(c(res$ref,res.est$ref)))
  
  return(comb.res)

}


###########################################
# Describe event time distribution with tables, figures, and a narrative

#' Describe Survival Variable
#'
#' @param x the time event data that is used for analysis.
#' @param tbl display the result in tabular format. The values are 0=none, 1=basic, 2=detailed.
#' @param fig display the result in figures. The values are 0=none, 1=basic, 2 and higher=more.
#' @param txt display narrative output. The values are 0=none, 1=basic, 2=detailed.
#' @param clr colors to use in displaying the figures.
#' @param x.name #ask_Stan
#'
#' @return returns event time distribution with tables, figures, and a narrative.
#' @export
#'
#' @examples #ask_Stan #no_need
#'
describe.event.timing=function(x,
                               tbl=1,
                               fig=1,
                               txt=1,
                               clr=NULL,
                               x.name=NULL)
{
  ######################################
  # check for proper input
  cls=class(x)
  if (!(any(cls%in%c("Surv","competing.events"))))
    stop("x must be of class Surv or competing.events.")


  # if nmx not provided then extract it from the input
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name


  # Kaplan-Meier curves
  if(any(cls%in%"Surv"))
  {
    km=survival::survfit(x~1)

    ylbl=paste0("Pr(",nmx,")")
    xlbl="time"

    if (fig>0)
    {
      temp.dset=cbind.data.frame(x=x)
      event.plot("x",temp.dset,y.name=nmx,clr=clr)
    }

    res.tbl=summary(km,times=pretty(c(0,max(x[,1]))))
    cum.event=cumsum(res.tbl$n.event)


    res.txt=NULL
    if (txt>0)
    {
      txt1=paste0("The event-time variable ",nmx," was observed for a cohort of ",
                  nrow(x)," subjects.  ",
                  "The data indicate that ",text.list(res.tbl$n.risk),
                  " subjects ",
                  "were respectively observed for least ",text.list(res.tbl$time),
                  " units of time without experiencing the ", nmx," event.  ")
      txt2=paste0("Also, ",text.list(cum.event)," subjects experience the ",nmx," event ",
                  "before times ",text.list(res.tbl$time),", respectively.")
      txt3=paste0("Based on this information, it is estimated that ",
                  text.list(paste0(round(100*res.tbl$surv,2),"%")),
                  " subjects in the population do not experience the ",nmx," event before times ",
                  text.list(res.tbl$time),", respectively.  ")

      res.txt=c(txt1,txt2,txt3)

    }

    method=paste0("The Kaplan-Meier (1958) method was used to estimate the distribution of ",
                  nmx," while accounting for censoring of event times.")

    ref='Kaplan, E. L.; Meier, P. (1958). "Nonparametric estimation from incomplete observations". J. Amer. Statist. Assoc. 53 (282): 457-481. doi:10.2307/2281868. JSTOR 2281868.'

    fnl.tbl=NULL

    if (tbl>0)
    {
      fnl.tbl=cbind.data.frame(time=res.tbl$time,
                               n.risk=res.tbl$n.risk,
                               n.event=res.tbl$n.event,
                               surv=res.tbl$surv)
    }
    if (tbl>1)
    {
      fnl.tbl$std.err=res.tbl$std.err
      fnl.tbl$CILB=res.tbl$lower
      fnl.tbl$CIUB=res.tbl$upper
    }



    res=list(tbl=fnl.tbl,
             txt=res.txt,
             method=method,
             ref=ref)

    class(res)="SBP.result"

    return(res)

  }


  if (any(cls%in%"competing.events"))
  {
    # Compute Cumulative Incidence Curve
    evnt.types=unique(x[,2])
    evnt.types=evnt.types[evnt.types!=0]
    clrs=define.colors(length(evnt.types),clr)
    ci=cmprsk::cuminc(x[,1],x[,2])

    # label the output
    names(ci)=substring(names(ci),3)
    ev.key=attr(x,"ev.key")
    for (i in 1:length(ev.key))
      names(ci)=gsub(names(ev.key)[i],ev.key[i],names(ci),fixed=T)

    # generate the plot
    xlbl="time"

    if (fig>0)
    {
      plot(ci,las=1,col=clrs,lty=1,lwd=2,
           xlab=xlbl,ylab="cumulative incidence")
    }

    res.tbl=cmprsk::timepoints(ci,times=pretty(c(0,max(x[,1]))))$est
    res.tbl=t(res.tbl)
    rw=rowSums(res.tbl)
    res.tbl=res.tbl[!is.na(rw),]

    for (i in 1:length(ev.key))
      colnames(res.tbl)=gsub(names(ev.key)[i],ev.key[i],
                             colnames(res.tbl),fixed=T)

    res.txt=NULL
    if (txt>0)
    {

      for (i in 1:ncol(res.tbl))
      {
        txt1=paste0("Based on this event timing data for ",nrow(x)," subjects, ",
                    "it is estimated that ")
        txt2=paste0(text.list(paste0(100*round(res.tbl[,i],2),"%"))," of subjects in the population ",
                    "experience ",colnames(res.tbl)[i]," before times ",text.list(rownames(res.tbl)),", respectively.")
        res.txt=c(res.txt,paste0(txt1,txt2))
      }
    }

    method=paste0("Gray's (1988) method was used to estimate the cumulative incidence of each type of ",
                  nmx," event as a function of time accounting for censoring and competing events.")

    ref="Gray RJ (1988) A class of K-sample tests for comparing the cumulative incidence of a competing risk, ANNALS OF STATISTICS, 16:1141-1154."


    na.row=which(rowSums(is.na(res.tbl))==ncol(res.tbl))
    if (length(na.row)>0)  res.tbl=res.tbl[-na.row,]

    res.tbl=cbind(time=as.numeric(rownames(res.tbl)),res.tbl)

    res=list(tbl=res.tbl,
             txt=res.txt,
             method=method,
             ref=ref)

    class(res)="SBP.result"

    return(res)
  }


}


##########################################
# Describe a categorical variable with tables, figures, and a narrative

#' Describe a categorical variable
#'
#' @param x the categorical variable to describe.
#' @param tbl display the result in tabular format. The values are 0=none, 1=basic, 2=detailed.
#' @param fig display the result in figures. The values are 0=none, 1=basic, 2 and higher=more.
#' @param txt display narrative output. The values are 0=none, 1=basic, 2=detailed.
#' @param clr colors to use in displaying the figures.
#' @param x.name name of x variable to use in narrative output
#' @param use.all indicates whether to include all data regardless of missingness. Default value is True.
#'
#' @return returns description of the categorical variable with tables, figures, and a narrative.
#' @export
#'
#' @examples #no_need
#'
describe.categorical=function(x,
                              tbl=1,
                              fig=1,
                              txt=1,
                              clr=NULL,
                              x.name=NULL,
                              use.all=T)

{
  ######################################
  # check for proper input
  cls=class(x)
  if (!any(cls%in%c("character","factor","ordered")))
    stop("non-categorical x.")


  ######################################
  # if nmx not provided then extract it from the input
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name

  ######################################
  # pick color scheme if not specified
  if (is.null(clr)) clr="rainbow"


  all.tbl=table(x,exclude=NULL)
  avl.tbl=table(x)

  n.miss=sum(all.tbl)-sum(avl.tbl)

  res.tbl=avl.tbl
  if (use.all) res.tbl=all.tbl

  pct.tbl=100*res.tbl/sum(res.tbl)

  res.txt=""

  ##############################################
  # produce figures

  temp.dset=cbind.data.frame(x=x)
  if (fig>0)
  {
    bar.plot("x",temp.dset,y.name=nmx,clr=clr,all=use.all)
    if (fig>1) pie.plot("x",temp.dset,nmx,clr=clr,all=use.all)
  }



  #######################################
  # Generate final table
  final.tbl=NULL
  final.tbl=cbind.data.frame(names(res.tbl),
                             n=as.vector(res.tbl),
                             percent=as.vector(pct.tbl))

  colnames(final.tbl)=c(nmx,"n","percent")


  ############################################
  # generate narrative

  res.txt=NULL
  if (txt>0)
  {
    res.txt=paste0("The categorical variable ",nmx," has ", length(x),
                   " observations:  ")
    n.txt=paste0(final.tbl[,"n"]," ",
                 final.tbl[,nmx],
                 " (",round(final.tbl[,"percent"],2),"%)")
    n.txt=paste0(n.txt,collapse=", ")
    n.txt=paste0(n.txt,".  ")
    res.txt=paste0(res.txt,n.txt)

    if ((!use.all)&(n.miss>0))
      res.txt=c(res.txt,
                paste0("This analysis ignores ",n.miss," missing data observations."))


  }



  if (tbl<1) final.tbl=NULL

  res=list(tbl=final.tbl,
           txt=res.txt,
           method=NULL,
           ref=NULL)

  class(res)="SBP.result"

  return(res)
}


#########################################
# Describe a numeric variable with tables, figures, and a narrative

#' Describe a numeric variable
#'
#' @param x the name of the variable in quotation marks.
#' @param tbl A flag that indicates to display the number of table(s). Default value is 1.
#' @param fig A flag that indicates to display the number of figure(s). Default value is 2.
#' @param txt A flag that indicates to display text. Default value is 1.
#' @param clr colors to use in displaying the plot(s).
#' @param x.name character string giving name of x variable
#'
#' @return returns a description of the numeric variable with tables, figures, and a narrative.
#' @export
#'
#' @examples #no_need
#'
describe.numeric=function(x,            # variable to describe
                          tbl=1,        # table: 0 = none, 1 = basic
                          fig=2,        # figure: 0 = none, 1 = boxplot, 2= boxplot+histogram, 3 = boxplot+histogram+qqnorm
                          txt=1,        # narrative detail (0 = none; 1 = basic; 2 = detailed)
                          clr=NULL,     # color to be used in the figures
                          x.name=NULL)  # character string giving name of x variable

{


  ######################################
  # check for proper input

  cls=class(x)
  if (!(cls%in%c("numeric","double","integer")))
    stop("non-numeric x.")

  ######################################
  # get name of x from input
  # if nmx not provided then extract it from the input
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name

  ######################################
  # pick color if not specified
  if (is.null(clr)) clr="gray"


  ######################################
  # descriptive statistics
  nml.test=normality.test(x)
  smry.stats=c(n.total=length(x),
               n.missing=sum(is.na(x)),
               n.available=sum(!is.na(x)),
               mean=mean(x,na.rm=T),
               stdev=stats::sd(x,na.rm=T),
               median=stats::median(x,na.rm=T),
               lower.quartile=stats::quantile(x,0.25,na.rm=T),
               upper.quartile=stats::quantile(x,0.75,na.rm=T),
               minimum=min(x,na.rm=T),
               maximum=max(x,na.rm=T),
               normality.pvalue=nml.test$p.value)
  names(smry.stats)=c("n.total","n.missing","n.available",
                      "mean","stdev","median",
                      "lower.quartile","upper.quartile",
                      "minimum","maximum","normality.pvalue")
  
  #########################################
  # Reformat x into a data set
  xlbl=nmx
  temp.dset=as.data.frame(x=x)
  colnames(temp.dset)=x.name
  
  ######################################
  # Outliers
  res.out = outliers(x.name,temp.dset,x.name,fig=0, txt, clr)
  
  ######################################
  # tables
  res.tbl=NULL
  if (tbl>0) res.tbl=smry.stats

  ####################################
  # figures
  xlbl=nmx
  temp.dset=as.data.frame(x=x)
  colnames(temp.dset)=x.name

  if (fig>0) box.plot(x.name,temp.dset,x.name,clr)
  if (fig>1) bar.plot(x.name,temp.dset,x.name,clr=clr)
  if (fig>2) outliers(x.name,temp.dset,x.name,txt=0,clr=clr,fig=1)
  if (fig>3) nqq.plot(x.name,temp.dset,x.name,clr=clr)

  ##############################################
  # narrative text
  dgts=ceiling(diff(range(log10(abs(as.numeric(unlist(x)))),na.rm=T)))
  res.txt=NULL
  if (txt>0)
  {
    res.txt=paste0("The variable ",nmx," has ", smry.stats["n.total"],
                   " observations (",
                   smry.stats["n.available"]," available; ",
                   smry.stats["n.missing"]," missing)  ",
                   "with mean ",round(smry.stats["mean"],dgts),
                   ", standard deviation ",round(smry.stats["stdev"],dgts),
                   ", median ",round(smry.stats["median"],dgts),
                   ", lower quartile ",round(smry.stats["lower.quartile"],dgts),
                   ", upper quartile ",round(smry.stats["upper.quartile"],dgts),
                   ", minimum ",round(smry.stats["minimum"],dgts),
                   ", and maximum ",round(smry.stats["maximum"],dgts),".  ")
    res.txt=c(res.txt,res.out$txt)
  }

  if (txt>1)
  {
    more.txt=paste0("There is ",
                    c("","not ")[1+(smry.stats["normality.pvalue"]>0.05)],
                    "statistically compelling evidence that ",
                    nmx," is not normally distributed (p = ",smry.stats["normality.pvalue"],").")
    res.txt=c(res.txt,more.txt)
  }

  if (txt>0) res.txt=paste0(res.txt,collapse="")

  method=paste0("The ",nml.test$method ," was used to evaluate the normality of the distribution of ",nmx,".  ")
  
  ref=res.out$ref
  if (grepl("Shapiro",nml.test$method))
    ref=c(ref,
        'Shapiro, S. S.; Wilk, M. B. (1965). "An analysis of variance test for normality (complete samples)". Biometrika. 52 (3-4): 591-611. doi:10.1093/biomet/52.3-4.591. JSTOR 2333709. MR 0205384.')
  
  if (grepl("Smirnov",nml.test$method))
    ref=c(ref,"George Marsaglia, Wai Wan Tsang and Jingbo Wang (2003). Evaluating Kolmogorov's distribution. Journal of Statistical Software, 8/18. doi:10.18637/jss.v008.i18.")
  
  



  res=list(tbl=res.tbl,
           txt=res.txt,
           method=method,
           ref=ref)

  attr(res,"result.type")="describe.numeric"
  attr(res,"result.name")=x.name

  class(res)="SBP.result"

  return(res)
}


###################################
# #ask_Stan

numeric.descriptive.table=function(x)

{
  res=describe.numeric(x,tbl=1,fig=0,txt=0)$tbl
  return(res)
}


################################
# Create a competing event time variable

#' Creating a competing event time variable
#' #probably needs a dataset as parameter.
#'
#' @param obs.time the time variable in the data set.
#' @param obs.event the event variable in the data set.
#' @param ev.key the event key. The default value is set to NULL.
#'
#' @return returns a new variable combining observation time and observation event
#' @export
#'
#' @examples #ask_Stan
#' data_frame <- data.frame(time = c(11.2, 8.2, 10.0, 27.3, 14.5, 26.4, 4.2, 15.2, 14.7, 10.4),
#'                          event = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0),
#'                          Age = c(50, 55, 45, 49, 52, 43, 62, 60, 70, 71))
#' competing.event.time("time","event")
#'
competing.event.time=function(obs.time,
                              obs.event,
                              ev.key=NULL)

{
  res=cbind(obs.time,obs.event)

  if (is.null(ev.key))
  {
    ev.key=sort(unique(obs.event))
    names(ev.key)=ev.key
  }

  attr(res,"ev.key")=ev.key
  class(res)=c("matrix","competing.events")
  return(res)
}


###########################################
# write a list of items as a narrative

#' Write list items as a narrative
#'
#' @param x the vector containing the list items.
#' @param ... other parameters
#'
#' @return returns the list items as a narrative.
#'
#' @export text.list
#' @export
#'
#' @examples
#' x=c("result 1","result 2","result 3")
#' text.list(x)
#'
text.list=function(x, ...)

{
  if (length(x)==1) return(x)
  if (length(x)==2) return(paste0(x,collapse=" and "))

  n=length(x)
  return(paste0(paste0(x[-n],collapse=", ")," and ",x[n]))
}

