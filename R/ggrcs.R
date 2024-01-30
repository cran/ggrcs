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
#'@importFrom "cowplot" "draw_label"
#'@import "cowplot"
#'@return a picture

#'@format NULL
#'@usage NULL
#'@export
#'@examples
#'library(rms)
#'library(ggplot2)
#'library(scales)
#'library(cowplot)
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
                         'guides scale_fill_discrete',
                         'draw_label',
                         'geom_hline',
                         'geom_density',
                         'scale_x_continuous',
                         'element_text',
                         'windowsFont'
))


ggrcs<-function(data,fit,x,group=NULL,groupcol=NULL,histlimit=NULL,histbinwidth=NULL,histcol=NULL,
                linetype=NULL,linesize=NULL,ribalpha=NULL,ribcol=NULL,xlab=NULL,ylab=NULL,
                leftaxislimit=NULL,lift=TRUE,P.Nonlinear=TRUE,liftname=NULL,riblinecol=NULL,linecol=NULL,
                title=NULL,px=NULL,py=NULL,two.group.label=NULL,histalpha=NULL,linealpha=NULL,
                bordercol=NULL,twotag.name=NULL,dec=NULL,fontsize=15,axis.text.size=15,fontfamily="serif",colset=NULL,
                breaks=NULL,pdensity=FALSE,limits=NULL,x.breaks=NULL,x.moiety=NULL,...){
  density<-NULL;yhat<-NULL;lower<-NULL;upper<-NULL
  if (missing(data)) {stop("data is miss.")}
  if (missing(fit)) {stop("fit is miss.")}
  if (length(x) < 1) { stop("No valid variables.")}
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
    pre0<-as.data.frame(pre0)
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
      } else {
        groupcol<-groupcol
      }}
    #####ggplot set
    if (missing(histlimit)) {histlimit<-c(yminlower,ymaxupper)} else {assign("histlimit",histlimit)}
    if (missing(histbinwidth)) {histbinwidth<-0.8} else {assign("histbinwidth",histbinwidth)}
    if (missing(histcol)) {histcol<-"plum1"} else {assign("histcol",histcol)}
    if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
    if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
    if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
    if (missing(ribcol)) {ribcol<-"lightblue3"} else {assign("ribcol",ribcol)}
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
    ######
    if (pdensity==FALSE) {
      p<-ggplot()+
        geom_histogram(data=pre0,aes(x=x,y =rescale(after_stat(density),histlimit),fill=group,group=group),
                       binwidth = histbinwidth,color=bordercol,alpha = histalpha)+
        scale_fill_manual(values=groupcol,labels = twotag.name)

    } else {
      p<-ggplot()+
        geom_density(data=pre0,aes(x=x,y =rescale(after_stat(density),histlimit),fill=group,group=group),
                     color=bordercol,alpha = histalpha)+
        scale_fill_manual(values=groupcol,labels = twotag.name)
    }
    ##############
    if (!missing(breaks) |!missing(x.breaks) |!missing(x.moiety)) {
      if (is.null(x.breaks)) {
        if (is.null(x.moiety)) {x.by<-breaks
        } else {
          x.by<-round(max(x)/x.moiety, 0)
        }
        breaks <- seq(0, max(x), by = x.by)
      } else {breaks<-x.breaks}
    }
    if (!missing(limits)) limits<-limits
    if (!missing(breaks) & !missing(limits)) {
      p<-p+scale_x_continuous(breaks = breaks, limits = limits)
    } else if (!missing(breaks) ) {
      p<-p+scale_x_continuous(breaks = breaks)
    } else if (!missing(limits)) {
      p<-p+scale_x_continuous(limits = limits)
    }
    ############
    if (missing(riblinecol)) {
      p<-p+geom_line(data=pre0,aes(x,yhat,colour=group,group=group),
                     linetype=linetype,linewidth=linesize,alpha = linealpha)+
        scale_colour_manual(values=groupcol)+
        geom_ribbon(data=pre0,aes(x=x,ymin=lower,ymax=upper,fill=group,group=group),alpha =ribalpha)+
        guides(colour = "none")
    } else {
      riblinecol<-riblinecol
      p<-p+geom_line(data=pre0,aes(x,yhat,colour=group,group=group),
                     linetype=linetype,linewidth=linesize,alpha = linealpha)+
        scale_colour_manual(values=groupcol)+
        geom_ribbon(data=pre0,aes(x=x,ymin=lower,ymax=upper,fill=group,group=group),alpha =ribalpha)+
        guides(colour = "none")
    }
    #########
    p<-p+theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(family = fontfamily,size = axis.text.size),
            axis.text.y = element_text(family = fontfamily,size = axis.text.size),
            axis.title.x = element_text(family = fontfamily,size = fontsize),
            axis.title.y = element_text(family = fontfamily,size = fontsize),
            text = element_text(family = fontfamily)
      )+
      xlab(xlab)+
      ylab(ylab)+
      labs(title=title)
  } else {
    #one group
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
    if ((!missing(colset))) {
      if (colset=="A")  {
        histcol<-c("darkseagreen1")
        ribcol<-c("gold1")
      }
      if (colset=="B")  {
        histcol<-c("darkseagreen1")
        ribcol<-c("burlywood3")
      }
      if (colset=="C")  {
        histcol<-c("cornsilk")
        ribcol<-c("palegreen")
      }
      if (colset=="D")  {
        histcol<-c("springgreen")
        ribcol<-c("tan3")
      }
    }
    #####ggplot set
    if (missing(histlimit)) {histlimit<-c(yminlower,ymaxupper)} else {assign("histlimit",histlimit)}
    if (missing(histbinwidth)) {histbinwidth<-0.8} else {assign("histbinwidth",histbinwidth)}
    if (missing(histcol)) {histcol<-"plum1"} else {assign("histcol",histcol)}
    if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
    if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
    if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
    if (missing(ribcol)) {ribcol<-"lightblue3"} else {assign("ribcol",ribcol)}
    if (missing(xlab)) {xlab<-x1} else {assign("xlab",xlab)}
    if (missing(ylab)) {ylab<-"Outcome Prediction Incidence"} else {assign("ylab",ylab)}
    if (missing(histalpha)) {histalpha<-0.5} else {assign("histalpha",histalpha)}
    if (missing(linealpha)) {linealpha<-0.7} else {assign("linealpha",linealpha)}
    if (missing(linecol)) {linecol<-ribcol} else {linecol<-linecol}
    if (missing(leftaxislimit)) {leftaxislimit<-c(dmin,dmax)} else {assign("leftaxislimit",leftaxislimit)}
    if (missing(title)) {title<-"The relationship between the variable and the predicted probability"} else {assign("title",title)}
    #############
    if (pdensity==FALSE) {
      p<-ggplot(pre0,aes(x=x))+
        geom_histogram(aes(x=x,y =rescale(after_stat(density),histlimit)),
                       binwidth = histbinwidth,fill=histcol,colour="black",alpha=histalpha)
    } else {
      p<-ggplot(pre0,aes(x=x))+
        geom_density(aes(x=x,y =rescale(after_stat(density),histlimit)),
                     fill=histcol,colour="black",alpha=histalpha)
    }
    #############
    if (!missing(breaks) |!missing(x.breaks) |!missing(x.moiety)) {
      if (is.null(x.breaks)) {
        if (is.null(x.moiety)) {x.by<-breaks
        } else {
          x.by<-round(max(x)/x.moiety, 0)
        }
        breaks <- seq(0, max(x), by = x.by)
      } else {breaks<-x.breaks}
    }
    if (!missing(limits)) limits<-limits
    if (!missing(breaks) & !missing(limits)) {
      p<-p+scale_x_continuous(breaks = breaks, limits = limits)
    } else if (!missing(breaks) ) {
      p<-p+scale_x_continuous(breaks = breaks)
    } else if (!missing(limits)) {
      p<-p+scale_x_continuous(limits = limits)
    }
    if (missing(riblinecol)) {
      p<-p+geom_line(data=pre0,aes(x,yhat),linetype=linetype,linewidth=linesize,alpha = linealpha,colour=linecol)+
        geom_ribbon(data=pre0,aes(ymin = lower, ymax = upper),alpha = ribalpha,fill=ribcol)
    } else {
      riblinecol<-riblinecol
      p<-p+geom_line(data=pre0,aes(x,yhat),linetype=linetype,linewidth=linesize,alpha = linealpha,colour=ribcol)+
        geom_ribbon(data=pre0,aes(ymin = lower, ymax = upper),
                    alpha = ribalpha,fill=ribcol,col=riblinecol)
    }
    p<-p+theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(family = fontfamily,size = axis.text.size),
            axis.text.y = element_text(family = fontfamily,size = axis.text.size),
            axis.title.x = element_text(family = fontfamily,size = fontsize),
            axis.title.y = element_text(family = fontfamily,size = fontsize),
            text = element_text(family = fontfamily)
      )+
      xlab(xlab)+
      ylab(ylab)+
      labs(title=title)
  }
  ###LIFT
  if (lift==TRUE) {
    if (missing(liftname)) {liftname<-"density"} else {assign("liftname",liftname)}
    p<-p+scale_y_continuous(sec.axis = sec_axis( ~rescale(.,leftaxislimit),
                                                 name = liftname))
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
    px<-round(px)
    py<-round(py)
    #px<-max(x)*0.3
    #py<-max(pre0$upper)*0.95
    p<-p+draw_label(text, size = fontsize,
                    fontfamily = fontfamily, x = px, y = py, hjust = 0,
                    vjust = 1)
  }
  p
}

