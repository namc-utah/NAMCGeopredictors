#' @title BaseClass_R6
#' @description R6 class for handling NAMC oAuth2
#' @return a `BaseClass_R6` class (R6 class)
#' @examples
#'
#'
BaseClass_R6 = R6Class(
  "BaseClass_R6",

  public = list(

    # ----------------------------------------------------------------
    # initialize - Allow list based function initialization (easier to integrate with config files)
    # ---------------------------------------------------------------
    initialize = function(argList=NULL,...){
      if(is.null(argList)) argList = list()
      private$.set_from_list( modifyList(argList, list(...)) )
    },


    #----------------------------------------------------------------
    # dump - for debugging internal class values (puts var named tmpVar in global environment)
    # ---------------------------------------------------------------
    dump = function(val){
      assign("tmpVar", val, envir = .GlobalEnv)
      return(val)
    }
  ),

  private = list(
    .silent = FALSE,

    # ----------------------------------------------------------------
    # .set_from_list - sets all public and private fields from a name,value list
    # ---------------------------------------------------------------
    .set_from_list = function(argList){
      if(length(argList) > 0){
        for(arg in names(argList)){
          if(exists(arg,self)){
            self[[arg]] = argList[[arg]]

          } else if (exists(arg,private)){
            private[[arg]] = argList[[arg]]

          }
        }
      }
    }
  )
)






#' @title Logger
#' @description R6 class for handling NAMC oAuth2
#' @return a `Logger` class (R6 class)
#' @examples
#'
#'
Logger = R6Class(
  "Logger",
  inherit = BaseClass_R6,

  public = list(
    logPath = NULL,
    fileName = NULL,
    appendDate = NULL,
    enabled = NULL,
    consoleOutput = NULL,
    started = NULL,

    #'----------------------------------------------------------------
    # initialize - analyzes the file upon creation
    #'---------------------------------------------------------------
    initialize = function(argList=NULL,...){
      super$initialize(argList,...)
      self$started = FALSE
    },

    toggleState = function() {
      self$enabled = !self$enabled
    },

    getFullFilePath = function() {
      file.path(self$logPath,self$fileName)
    },

    startLog = function( forceLog = FALSE, suffix = '', prefix = '' ){
      if(self$enabled || forceLog) {
        if(is.null(self$fileName) || nchar(self$fileName) == 0){
          self$fileName = paste( prefix, format(Sys.time(), format='%Y-%m-%d_%H.%M.%S'), suffix, ".log", sep = "")
        }

        # Log all Output
        dir.create( path = self$logPath, recursive = TRUE, showWarnings = FALSE )
        sink(
          file = file.path( self$logPath, self$fileName ),
          type = "output",
          split = self$consoleOutput
        )
        # Start timer
        tictoc::tic.clear()
        tictoc::tic.clearlog()
        tictoc::tic(msg = "Completed In")

        self$started = TRUE
      }
    },

    stopLog = function(){
      if(self$started){
        tictoc::toc()
        sink(file = NULL,type = "output")
        self$started = FALSE
      }
    }

  )
)
