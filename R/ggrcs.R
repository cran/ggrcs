#'@title  ggrcs
#'@name  ggrcs
#'@description  A Function to Draw Histograms and Restricted Cubic Splines (RCS)
#'
#'@details  You can use this function to easily draw a combined.histogram and restricted cubic spline.The function draws the graph through ggplot2.RCS fitting requires the use of the rcs function of the RMS package.Can fit cox regression,logistic regression and linear regression models.
#'
#'
#'@param data need a dataframe
#'@param fit  You need the fitted model.Must be  lrm or coxph.
#'@param x The target variable you wish to fit. It is displayed on the X-axis when plotting.
#'@importFrom "stats" "density"
#'@importFrom "rms" "Predict"
#'@importFrom "ggplot2" "aes" "element_blank" "geom_histogram" "geom_line" "geom_ribbon" "ggplot" "labs" "scale_y_continuous"
#'@importFrom "scales" "rescale"
#'@importFrom "stats" "anova"
#'
#'@return a picture

#'@format NULL
#'@usage NULL
#'@export
#'@examples library(rms)
#'library(ggplot2)
#'library(scales)
#'dt<-smoke
#'dd<-datadist(dt)
#'options(datadist='dd')
#'fit<- cph(Surv(time,status==1) ~ rcs(age,4)+gender, x=TRUE, y=TRUE,data=dt)
#'###single group
#'ggrcs(data=dt,fit=fit,x="age")
#'##two groups
#'ggrcs(data=dt,fit=fit,x="age",group="gender")


utils::globalVariables(c('theme_bw',
                         'theme',
                         'sec_axis',
                         'scale_fill_manual',
                         'scale_colour_manual',
                         'annotate',
                         'after_stat',
                         'scale_fill_discrete',
                         'guides',
                         'guides scale_fill_discrete'
))


ggrcs<-function(data,fit,x,group=NULL,groupcol=NULL,histlimit=NULL,histbinwidth=NULL,histcol=NULL,
                linetype=NULL,linesize=NULL,ribalpha=NULL,ribcol=NULL,xlab=NULL,ylab=NULL,
                leftaxislimit=NULL,lift=TRUE,Pvalue=NULL,P.Nonlinear=FALSE,liftname=NULL,
                title=NULL,xP.Nonlinear=NULL,yP.Nonlinear=NULL,two.group.label=NULL,
                bordercol=NULL,twotag.name=NULL,...){
  density<-NULL;yhat<-NULL;lower<-NULL;upper<-NULL
  if (missing(data)) {stop("data is miss.")}
  if (missing(fit)) {stop("fit is miss.")}
  if (length(x) < 1) { stop("No valid variables.")}
  if (is.data.frame(data) == FALSE) {
    stop("The data argument needs to be a data frame (no quote).")
  }
  call <- match.call()
  data<-as.data.frame(data)
  fit <- fit;assign("fit", fit);
  if (!missing(group)) {assign("group",group)}
  if (!missing(group)) {
    #two group
    an<-anova(fit)
    P.value<-an[2,3]
    P.value<-round(P.value,3)
    dt<-as.data.frame(data)
    x1<-x #get name
    x<-dt[,x]#predect data x
    gro<-dt[,group]
    pre0<-predata(fit=fit,variables=x1,y=x,group=group)
    pre0[,group]<-as.numeric(as.factor(pre0[,group]))
    pre0[,group]<-as.factor(pre0[,group])
    names(pre0)[names(pre0) == group] <- "group"
    x<-pre0[,x1] #plot x tow double data
    ####data set
    d<-density(x)
    dmin<-as.numeric(min(d[["y"]]))##density
    dmax<-as.numeric(max(d[["y"]]))##density
    yminlower<-as.numeric(min(pre0$lower))
    ymaxupper<-as.numeric(max(yhat1<-pre0$upper))
    #####ggplot set
    if (missing(histlimit)) {histlimit<-c(yminlower,ymaxupper)} else {assign("histlimit",histlimit)}
    if (missing(histbinwidth)) {histbinwidth<-0.8} else {assign("histbinwidth",histbinwidth)}
    if (missing(histcol)) {histcol<-"green"} else {assign("histcol",histcol)}
    if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
    if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
    if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
    if (missing(ribcol)) {ribcol<-"red"} else {assign("ribcol",ribcol)}
    if (missing(Pvalue)) {Pvalue<-P.value} else {assign("Pvalue",Pvalue)}
    if (missing(xlab)) {xlab<-x1} else {assign("xlab",xlab)}
    if (missing(ylab)) {ylab<-"Outcome Prediction Incidence"} else {assign("ylab",ylab)}
    if (missing(leftaxislimit)) {leftaxislimit<-c(dmin,dmax)} else {assign("leftaxislimit",leftaxislimit)}
    if (missing(title)) {title<-"The relationship between the variable and the predicted probability"} else {assign("title",title)}
    if (!missing(groupcol)) {assign("groupcol",groupcol)}
    if (missing(twotag.name)) {twotag.name<-c("1","2")} else {assign("twotag.name",twotag.name)}
    if (missing(bordercol)) {bordercol<-"black"} else {assign("bordercol",bordercol)}
    if (missing(groupcol)) {
      P<-ggplot()+
        geom_histogram(data=pre0,aes(x=x,y =rescale(after_stat(density),histlimit),fill=group,group=group),
                       binwidth = histbinwidth,color=bordercol)+scale_fill_discrete(labels = twotag.name)+
        geom_line(data=pre0,aes(x,yhat,colour=group,group=group),linetype=linetype,size=linesize,alpha = 0.9)+guides(colour = "none")+
        geom_ribbon(data=pre0,aes(x=x,ymin=lower,ymax=upper,fill=group,group=group),alpha =ribalpha)+
        theme_bw()+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        xlab(xlab)+
        ylab(ylab)+
        labs(title=title)
    } else {
      P<-ggplot()+
        geom_histogram(data=pre0,aes(x=x,y =rescale(after_stat(density),histlimit),fill=group,group=group),
                       binwidth = histbinwidth,color=bordercol)+
        scale_fill_manual(values=groupcol,labels = twotag.name)+
        geom_line(data=pre0,aes(x,yhat,colour=group,group=group),linetype=linetype,size=linesize,alpha = 0.9)+
        scale_colour_manual(values=groupcol)+
        geom_ribbon(data=pre0,aes(x=x,ymin=lower,ymax=upper,fill=group,group=group),alpha =ribalpha)+guides(colour = "none")+
        theme_bw()+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        xlab(xlab)+
        ylab(ylab)+
        labs(title=title)
    }
  } else {
    #one group
    an<-anova(fit)
    P.value<-an[2,3]
    P.value<-round(P.value,3)
    dt<-data
    x1<-x
    x<-dt[,x]
    pre0 <-predata(fit=fit,variables=x1,y=x)
    pre0<-as.data.frame(pre0)
    ####data set
    d<-density(x)
    dmin<-as.numeric(min(d[["y"]]))##density
    dmax<-as.numeric(max(d[["y"]]))##density
    yminlower<-as.numeric(min(pre0$lower))
    ymaxupper<-as.numeric(max(yhat1<-pre0$upper))
    #####ggplot set
    if (missing(histlimit)) {histlimit<-c(yminlower,ymaxupper)} else {assign("histlimit",histlimit)}
    if (missing(histbinwidth)) {histbinwidth<-0.8} else {assign("histbinwidth",histbinwidth)}
    if (missing(histcol)) {histcol<-"green"} else {assign("histcol",histcol)}
    if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
    if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
    if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
    if (missing(ribcol)) {ribcol<-"red"} else {assign("ribcol",ribcol)}
    if (missing(Pvalue)) {Pvalue<-P.value} else {assign("Pvalue",Pvalue)}
    if (missing(xlab)) {xlab<-x1} else {assign("xlab",xlab)}
    if (missing(ylab)) {ylab<-"Outcome Prediction Incidence"} else {assign("ylab",ylab)}
    if (missing(leftaxislimit)) {leftaxislimit<-c(dmin,dmax)} else {assign("leftaxislimit",leftaxislimit)}
    if (missing(title)) {title<-"The relationship between the variable and the predicted probability"} else {assign("title",title)}
    P<-ggplot(pre0,aes(x=x))+
      geom_histogram(aes(x=x,y =rescale(after_stat(density),histlimit)),binwidth = histbinwidth,fill=histcol,colour="black")+
      geom_line(data=pre0,aes(x,yhat),linetype=linetype,size=linesize,alpha = 0.9,colour=ribcol)+
      geom_ribbon(data=pre0,aes(ymin = lower, ymax = upper),alpha = ribalpha,fill=ribcol)+
      theme_bw()+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      xlab(xlab)+
      ylab(ylab)+
      labs(title=title)
  }
  ###LIFT
  if (lift==TRUE) {
    if (missing(liftname)) {liftname<-"density"} else {assign("liftname",liftname)}
    P<-P+scale_y_continuous(sec.axis = sec_axis( ~rescale(.,leftaxislimit),
                                                 name = liftname))
  }
  if (P.Nonlinear==TRUE) {
    text<- ""
    text<- paste("P for Nonlinear:",Pvalue,sep="")
    if (missing(xP.Nonlinear)) {xP.Nonlinear<-max(x)*0.3} else {assign("xP.Nonlinear",xP.Nonlinear)}
    if (missing(yP.Nonlinear)) {yP.Nonlinear<-max(pre0$upper)*0.95} else {assign("yP.Nonlinear",yP.Nonlinear)}
    #xP.Nonlinear<-max(x)*0.3
    #yP.Nonlinear<-max(pre0$upper)*0.95
    P<-P+annotate("text", x=xP.Nonlinear,y=yP.Nonlinear,label=text,size=5)
  }
  P
}

