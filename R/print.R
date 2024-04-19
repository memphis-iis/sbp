#'
#' Print a function's result in a narrative
#'
#' The function `print.SBP.result()` prints the result of a function in a properly narrated format.
#'
#' @param x The variable that stores the calculation results.
# #TODO: not sure how to document ellipsis arguments; currently using examples to do this in part
# @param knt A flag that identifies if knitr is available. Default value is set to TRUE. #ask_Stan
# @param tbl A flag to show tables in the result. Default value is set to TRUE.
# @param txt A flag to show texts in the result. Default value is set to TRUE.
# @param method #ask_Stan
# @param ref A flag to show references in the result. Default value is set to TRUE.
#' @param ... other parameters passed to SBP print method.
#'
#' @return shows a functions results containing tables, results, methods and references.
#' @export
#'
#' @examples
#' library("penalized")
#' data(nki70)
#' grade=describe("Grade",nki70)
#' # knt A flag that identifies if knitr is available. Default value is set to TRUE.
#' # tbl A flag to show tables in the result. Default value is set to TRUE.
#' # txt A flag to show texts in the result. Default value is set to TRUE.
#' # method A flag to show the method. Default value set to TRUE
#' # ref A flag to show references in the result. Default value is set to TRUE.
#' print(grade,knt=TRUE,tbl=TRUE,txt=TRUE,method=FALSE,ref=FALSE)
#'
print.SBP.result=function(x, ...)
{
  # remap x
  sbp.result = x

  # return all outputs by default
  knt = TRUE
  tbl=TRUE
  txt=TRUE
  method=TRUE
  ref=TRUE

  # unpack passed arguments from ...
  arguments <- list(...)
  argument_names <- names(arguments)
  if( "knt" %in% argument_names ){
    knt <- arguments$knt
  }
  if( "tbl" %in% argument_names ){
    tbl <- arguments$knt
  }
  if( "txt" %in% argument_names ){
    txt <- arguments$knt
  }
  if( "method" %in% argument_names ){
    method <- arguments$knt
  }
  if( "ref" %in% argument_names ){
    ref <- arguments$knt
  }

  if ((!is.null(sbp.result$tbl))&&(tbl))
  {
    write(" **TABLES** \n ",file="")
    tbl.res=sbp.result$tbl
    if (is.data.frame(tbl.res)) tbl.res=list(tbl=sbp.result$tbl)
    if (!is.list(tbl.res)) tbl.res=list(tbl=sbp.result$tbl)
    n.tbl=length(tbl.res)
    for (i in 1:n.tbl)
    {
      if (!is.null(tbl.res[[i]]))
      {
        if (knt)
        {
          res.name=NA
          if (is.numeric(tbl.res[[i]])&&(!is.null(attr(sbp.result,"result.name"))))
            res.name=attr(sbp.result,"result.name")
          write(knitr::kable(tbl.res[[i]],col.names=res.name),file="")
        }

        else print(tbl.res[[i]])
        write("\n ",file="")
      }
    }
  }


  if ((!is.null(sbp.result$txt))&&(txt))
  {
    write("\n **RESULTS** \n ",file="")

    write(paste(sbp.result$txt,collapse=""),file="")
  }

  if ((!is.null(sbp.result$method))&&(method))
  {
    write("\n \n **METHODS** \n ",file="")
    write(paste(sbp.result$method,collapse=""),file="")
  }


  if ((!is.null(sbp.result$ref))&&(ref))
  {
    write("\n \n **REFERENCES** \n ",file="")
    write(paste(sbp.result$ref,collapse="\n \n"),file="")
  }

}

#' Paste the table results in a word file
#'
#' #ask_stan
#'
#' @param SBP.result the format of the result.
#'
#' @return outputs the result tables in a word file.
#' @export
#'
#' @examples
#' #ask_Stan
#'
word.table=function(SBP.result)

{
  if (!is.null(SBP.result$tbl))
  {
    tbl.res=SBP.result$tbl
    if (is.data.frame(tbl.res)) tbl.res=list(tbl=SBP.result$tbl)
    if (!is.list(tbl.res)) tbl.res=list(tbl=SBP.result$tbl)
    n.tbl=length(tbl.res)
    for (i in 1:n.tbl)
    {
      if (!is.null(tbl.res[[i]]))
      {
        res.name=NA
          if (is.numeric(tbl.res[[i]])&&(!is.null(attr(SBP.result,"result.name"))))
            res.name=attr(SBP.result,"result.name")
          write.table(tbl.res[[i]],col.names=res.name,quote=F,file="",sep=",")
      }
      write("\n \n",file="")
    }
    write("**INSTRUCTIONS**",file="")
    write("For each section of output above:")
    write("1. Copy the output into Word.",file="")
    write("2. Highlight the output in Word.",file="")
    write("3. Go to Insert>Table>Convert Text to Table.",file="")
  }

}

#' Print SBP results in power point slides
#'
#' The function `SBP.example.Rmd.slides()` prints the SBP formatted result in power point slides.
#' #ask_stan
#'
#' @param SBP.code.string #ask_Stan
#' @param slide.title #ask_Stan
#'
#' @return writes the SBP results in power point slides
#' @export
#'
#' @examples #ask_Stan
#'
#'
SBP.example.Rmd.slides=function(SBP.code.string,slide.title)

{
  write(paste0("## ",slide.title,"\n "),file="")
  write("```{r,results='hide'}",file="")
  write(SBP.code.string,file="")
  write("``` \n",file="")

  eq.pos=regexpr("=",SBP.code.string)
  res.name=substring(SBP.code.string,1,eq.pos-1)

  write(paste0("## ",slide.title,"\n "),file="")
  write("```{r,eval=F,echo=T}",file="")
  write(res.name,file="")
  write("``` \n",file="")

  write("```{r,eval=T,echo=F}",file="")
  write(paste0("print.SBP.result(",res.name,",method=F,ref=F)"),file="")
  write("``` \n",file="")

  write(paste0("## ",slide.title,"\n "),file="")

  write("```{r,eval=T,echo=F}",file="")
  write(paste0("print.SBP.result(",res.name,",txt=F,tbl=F)"),file="")
  write("``` \n",file="")

  write(paste0("## ",slide.title,"\n "),file="")

  write("```{r,eval=T,echo=F,results='asis'}",file="")
  write(paste0("print.SBP.result(",res.name,",method=F,ref=F)"),file="")
  write("``` \n",file="")

  write(paste0("## ",slide.title,"\n "),file="")

  write("```{r,eval=T,echo=F,results='asis'}",file="")
  write(paste0("print.SBP.result(",res.name,",txt=F,tbl=F)"),file="")
  write("``` \n",file="")

}
