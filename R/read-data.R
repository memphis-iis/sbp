#' Read a data file
#'
#' The function `read.data()` reads data files of different extensions(csv, txt, Rdata, xlsx) and returns
#' the data as a data frame.
#'
#' @param file.name the name of the file with extension.
#'
#' @return returns the data of the file as a data frame.
#' @export
#'
#' @examples
#' csv=system.file("extdata", "gre_data.csv", package = "sbp")
#' read.data(csv)
#'
read.data=function(file.name=NULL)

{
  ##################################
  # Interactively choose file if no file is provided
  if (is.null(file.name))
    file.name=file.choose()

  ##############################################
  # Determine whether the file exists
  real.file=file.exists(file.name)

  if (!real.file)
  {
    alert(file.name,"does not exist.  Please choose file interactively.")
    file.name=file.choose()
  }
  R.file.name=gsub('\\','/',file.name,fixed=T)

  ######################################
  # Determine the type of data
  file.type=file.extension(file.name)
  alert(basename(file.name)," is a ",file.type," file.")

  ######################################
  # Read a csv or txt file
  if (file.type%in%c("csv","txt"))
  {
    dset=try(data.table::fread(file.name))
    dset=as.data.frame(dset)
    try(View(dset))
    alert("Next time you may use the following R command:")
    alert('my.data=read.data("',R.file.name,'")')
    return(dset)
  }

  ####################################
  # load an R data file
  if (file.type=="Rdata")
  {
    loaded.objects=try(load(file.name,verbose=T))
    for (i in 1:length(loaded.objects))
    {
      R.code=paste0("try(View(",loaded.objects[i],"))")
      eval(parse(text=R.code))
    }
    alert("Successfully obtained ",loaded.objects," from ",basename(file.name),".")
    alert("Next time you may use the following R command:")
    alert('load("',R.file.name,'",verbose=T)')
    alert("Use View(data.object.name) to see the data in the viewer.")
    return(invisible())
  }


  ###################################
  # read an xlsx file
  if (file.type=="xlsx")
  {
    file.sheets=readxl::excel_sheets(file.name)
    sheet.choice=try(select.list(file.sheets,
                             title="Choose the excel sheet to read",
                             graphics=T))
    # if (class(sheet.choice)=="try-error") #TODO: delete if below code works
    if( is( sheet.choice, "try-error"))
    {
      sheet.choice=select.list(file.sheets,
                               title="Choose the excel sheet to read.",
                               graphics=F)
    }
    dset=try(readxl::read_xlsx(file.name,sheet=sheet.choice))
    dset=as.data.frame(dset)
    try(View(dset))
    # if (class(dset)=="data.frame") #TODO: delete if below code works
    if( is( dset, "data.frame"))
    {
      alert("Successfully read sheet 1 of ",basename(file.name),".")
      alert("Next time you may use the following R code:")
      alert('my.data=read.data("',R.file.name,'")')
      return(dset)
    }
  }

  #############################
  # If all of the above fails

  alert("Data not read.")
  alert("Please specify an Rdata, xlsx, txt (tab delimited text) or csv file.")
  stop("No data read.")
}

##################################

#' Extract a file extension type
#'
#' The function `file.extension()` reads a data file and returns its file type.
#'
#' @param file.name the name of the file with extension
#'
#' @return returns the extension name of the file
#' @export
#'
#' @examples
#' file.extension("binary.csv")
#'
file.extension=function(file.name)
{
  split.name=unlist(strsplit(file.name,split=".",fixed=T))
  return(split.name[length(split.name)])
}


####################################
# Alert the user with a message

#' User Alert Message
#'
#' The function `alert()` takes an input string and prints it as an alert message.
#'
#' @param ... the alert message as string in the function
#'
#' @return prints the alert message.
#' @export
#'
#' @examples
#' alert("Encountered a problem")
#'
alert=function(...)
{
  alert.message=paste0(...)
  message(alert.message)
}

####################################
# Alert the user and stop the calculation

#' Alert user and stop calculation
#'
#' #this function halts the package building process. Should be removed.
#' #ask_stan
#'
#' @param ... the error message as string in the function
#'
#' @return print the alert message and stops execution of program.
#' @export
#'
#' @examples ##no_need
#'
stop.alert=function(...)
{
  alert.message=paste0(...)
  stop(alert.message)
}
