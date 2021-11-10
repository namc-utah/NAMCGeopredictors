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
