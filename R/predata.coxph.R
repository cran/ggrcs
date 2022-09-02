#'@title  predata.coxph
#'@description Generate the predicted data for the function. This is needed for drawing.
#'@param fit Model function required for prediction.
#'@param variables variable name.
#'@param y the value of the variable.
#'@param group Variables that need to be grouped.
#'@return Data required for plotting.
#'
predata.coxph<-function(fit,variables,y,group=NULL){
  Pre0<-NULL
  if (missing(group)){
    assign("fit", fit)
    x<-"x"
    formula <- ""
    formula <- paste(formula,variables,sep="")
    formula <- paste(formula,"=",x,sep="")
    formula <- paste(" Pre0 <- rms::Predict(fit, ",
                     formula,sep="")
    formula <- paste(formula,",fun=exp,ref.zero=T,conf.int = 0.95,digits=2)")
    call1<-call(formula)
    x<-y
    eval(parse(text=call1))
  } else {
    {assign("group",group)}
    x<-"x"
    #x1<-x1
    formula <- ""
    formula <- paste(formula,variables,sep="")
    formula <- paste(formula,"=",x,sep="")
    formula <- paste(" Pre0 <- rms::Predict(fit, ",
                     formula,",",sep="")
    formula <- paste(formula,group)
    formula <- paste(formula,",fun=exp")
    formula <- paste(formula,",ref.zero=T,conf.int = 0.95,digits=2)")
    call1<-call(formula)
    x<-y
    eval(parse(text=call1))
  }
  return(Pre0)
}
