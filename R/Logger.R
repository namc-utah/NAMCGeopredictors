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
