#'@title  singlercs
#'@name  singlercs
#'@description  A Function to Draw Restricted Cubic Splines (RCS)
#'
#'@details  You can use this function to easily draw a restricted cubic spline.The function draws the graph through ggplot2.RCS fitting requires the use of the rcs function of the RMS package.Can fit cox regression,logistic regression and linear regression models.
#'
#'
#'@param data need a dataframe
#'@param fit  You need the fitted model.Must be  lrm , ols or coxph.
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
#'###one group
#'singlercs(data=dt,fit=fit,x="age")
#'##two groups
#'singlercs(data=dt,fit=fit,x="age",group="gender")


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


singlercs<-function(data,fit,x,group=NULL,groupcol=NULL,histlimit=NULL,histbinwidth=NULL,histcol=NULL,
                    linetype=NULL,linesize=NULL,ribalpha=NULL,ribcol=NULL,xlab=NULL,ylab=NULL,
                    leftaxislimit=NULL,lift=TRUE,P.Nonlinear=TRUE,liftname=NULL,gethline=TRUE,
                    title=NULL,px=NULL,py=NULL,two.group.label=NULL,histalpha=NULL,linealpha=NULL,linecol=NULL,
                    bordercol=NULL,twotag.name=NULL,dec=NULL,fontsize=12,fontfamily="serif",colset=NULL,...){
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
  if (is.null(dec)) {dec<-3} else {dec<-dec}
  an<-anova(fit)
  if (any(class(fit)=="cph"|class(fit)=="lrm")==T) {
    p.value <- an[2, 3]
    p.overall <- an[1, 3]
  } else {
    p.value <- an[2, 5]
    p.overall <- an[1, 5]
  }
  p.value<-pvformat(p.value,dec)
  p.overall<-pvformat(p.overall,dec)
  if (!missing(group)) {
    assign("group",group)
    groupname<-levels(factor(data[,group]))
  }
  if (!missing(group)) {
    #two group
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
    if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
    if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
    if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
    if (missing(ribcol)) {ribcol<-"red"} else {assign("ribcol",ribcol)}
    if (missing(xlab)) {xlab<-x1} else {assign("xlab",xlab)}
    if (missing(ylab)) {ylab<-"Outcome Prediction Incidence"} else {assign("ylab",ylab)}
    if (missing(histalpha)) {histalpha<-0.5} else {assign("histalpha",histalpha)}
    if (missing(linealpha)) {linealpha<-0.7} else {assign("linealpha",linealpha)}
    if (missing(linecol)) {linecol<-ribcol} else {linecol<-linecol}
    if (missing(leftaxislimit)) {leftaxislimit<-c(dmin,dmax)} else {assign("leftaxislimit",leftaxislimit)}
    if (missing(title)) {title<-"The relationship between the variable and the predicted probability"} else {assign("title",title)}
    if (!missing(groupcol)) {assign("groupcol",groupcol)}
    if (missing(twotag.name)) {twotag.name<-groupname} else {assign("twotag.name",twotag.name)}
    if (missing(bordercol)) {bordercol<-"black"} else {assign("bordercol",bordercol)}
    if (missing(groupcol)) {
      groupcol=c("plum1","lightblue3")
      if ((!missing(colset))) {
        if (colset=="A")  {
          groupcol<-c("darkseagreen1","gold1")
        }
        if (colset=="B")  {
          groupcol<-c("darkseagreen1","burlywood3")
        }
        if (colset=="C")  {
          groupcol<-c("cornsilk","palegreen")
        }
        if (colset=="D")  {
          groupcol<-c("springgreen","tan3")
        }
      }
      P<-ggplot()+
        geom_line(data=pre0,aes(x,yhat,group=group,col=group),linetype=linetype,
                  linewidth=linesize,alpha = linealpha)+
        scale_colour_manual(values=groupcol,labels = twotag.name)+
        geom_ribbon(data=pre0,aes(x,ymin = lower, ymax = upper,fill=group),alpha = ribalpha)+
        scale_fill_manual(values=groupcol)+
        guides(colour = "none")+
        theme_bw()+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        xlab(xlab)+
        ylab(ylab)+
        labs(title=title)
    } else {
      P<-ggplot()+
        geom_line(data=pre0,aes(x,yhat,colour=group,group=group),linetype=linetype,
                  linewidth=linesize,alpha = linealpha)+
        scale_colour_manual(values=groupcol)+
        geom_ribbon(data=pre0,aes(x=x,ymin=lower,ymax=upper,fill=group,group=group),alpha =ribalpha)+
        guides(colour = "none")+
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
    #####ggplot set
    if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
    if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
    if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
    if (missing(ribcol)) {ribcol<-"plum1"} else {assign("ribcol",ribcol)}
    if (missing(xlab)) {xlab<-x1} else {assign("xlab",xlab)}
    if (missing(ylab)) {ylab<-"Outcome Prediction Incidence"} else {assign("ylab",ylab)}
    if (missing(histalpha)) {histalpha<-0.5} else {assign("histalpha",histalpha)}
    if (missing(linealpha)) {linealpha<-0.7} else {assign("linealpha",linealpha)}
    if (missing(linecol)) {linecol<-ribcol} else {linecol<-linecol}
    if (missing(title)) {title<-"The relationship between the variable and the predicted probability"} else {assign("title",title)}
    if ((!missing(colset))) {
      if (colset=="A")  {
        ribcol<-c("gold1")
        linecol<-c("gold1")
      }
      if (colset=="B")  {
        ribcol<-c("burlywood3")
        linecol<-c("burlywood3")
      }
      if (colset=="C")  {
        ribcol<-c("palegreen")
        linecol<-c("palegreen")
      }
      if (colset=="D")  {
        ribcol<-c("tan3")
        linecol<-c("tan3")
      }
    }
    P<-ggplot(pre0,aes(x=x))+
      geom_line(data=pre0,aes(x,yhat),linetype=linetype,linewidth=linesize,
                alpha = linealpha,colour=linecol)+
      geom_ribbon(data=pre0,aes(ymin = lower, ymax = upper),alpha = ribalpha,fill=ribcol)+
      theme_bw()+
      theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      xlab(xlab)+
      ylab(ylab)+
      labs(title=title)
  }
  if (P.Nonlinear==TRUE) {
    if (any(grepl("<",p.value))) {
      p.value <- paste0("P for nonlinear", p.value)
    }
    else {
      p.value <- paste0("P for nonlinear", " = ",
                        p.value)
    }
    if (any(grepl("<",p.overall))) {
      p.overall <- paste0("P for overall", p.overall)
    }
    else {
      p.overall <- paste0("P for overall", " = ",
                          p.overall)
    }
    text<- ""
    text<- paste(p.overall, p.value, sep = "\n")
    if (missing(px)) {px<-max(x)*0.3} else {assign("px",px)}
    if (missing(py)) {py<-max(pre0$upper)*0.95} else {assign("py",py)}
    #px<-max(x)*0.3
    #py<-max(pre0$upper)*0.95
    P<-P+draw_label(text, size = fontsize,
                    fontfamily = fontfamily, x = px, y = py, hjust = 0,
                    vjust = 1)
  }
  if (gethline) {
    if (any(class(fit)=="cph"|class(fit)=="lrm")==T) {
      yintercept<-1
    } else {
      yintercept<-0
    }
    P<-P+geom_hline(yintercept=yintercept, linetype=2,linewidth=0.5)
  }
  P
}
