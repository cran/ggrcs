#'@title  ggrcs2
#'@name  ggrcs2
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
#'@importFrom "graphics" "hist"
#'@importFrom "stats" "anova"
#'@importFrom "cowplot" "draw_label"
#'@import "cowplot"
#'@return a picture

#'@format NULL
#'@usage NULL
#'@export



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
                         'windowsFont',
                         'hist',
                         'geom_bar'
))

ggrcs2<-function(fit,data,x,group=NULL,groupcol=NULL,histlimit=NULL,histbinwidth=NULL,histcol=NULL,
                linetype=NULL,linesize=NULL,ribalpha=NULL,ribcol=NULL,xlab=NULL,ylab=NULL,
                leftaxislimit=NULL,lift=TRUE,P.Nonlinear=TRUE,liftname=NULL,riblinecol=NULL,linecol=NULL,
                title=NULL,px=NULL,py=NULL,two.group.label=NULL,histalpha=NULL,linealpha=NULL,
                bordercol=NULL,twotag.name=NULL,dec=NULL,fontsize=15,axis.text.size=15,fontfamily="serif",colset=NULL,
                pdensity=FALSE,x.limits=NULL,y.limits=NULL,x.breaks=NULL,x.moiety=NULL,breaks=NULL,ymax.l=NULL,
                n.hist=NULL,...){
  density<-NULL;yhat<-NULL;lower<-NULL;upper<-NULL;pct<-NULL
  if (missing(data)) {stop("data is miss.")}
  if (missing(fit)) {stop("fit is miss.")}
  if (length(x) < 1) { stop("No valid variables.")}
  call <- match.call()
  data<-as.data.frame(data)
  fit <- fit;
  if (is.null(dec)) {dec<-3} else {dec<-dec}
  an<-anova(fit)
  an<-as.data.frame(an)
  if (any(class(fit)=="cph"|class(fit)=="lrm")==T) {
    p.value <- an[" Nonlinear", 3]
    p.overall <- an[x, 3]
  } else {
    p.value <- an[" Nonlinear", 5]
    p.overall <- an[x, 5]
  }
  p.value<-pvformat(p.value,dec)
  p.overall<-pvformat(p.overall,dec)
  if (!missing(group)) {
    assign("group",group)
    groupname<-levels(factor(data[,group]))
  }
  if (is.null(group)) {
    #one group
    dt<-data
    x.name<-x
    x<-dt[,x]
    pre0 <-predata(fit=fit,variables=x.name,y=x)
    pre0<-as.data.frame(pre0)
    ####data set
    xmin <- min(dt[, x.name])
    xmax <- max(dt[, x.name])
    yminlower<-as.numeric(floor(min(pre0$lower)))
    ymaxupper<-as.numeric(ceiling(max(pre0$upper)))
    if (is.null(n.hist)) {
      n.hist<-20
    } else {n.hist<-n.hist}
    hist.breaks <- seq(xmin, xmax, length=n.hist)
    h <- hist(dt[, x.name], breaks=hist.breaks, right=TRUE)
    h.breaks <- seq(xmin, xmax, length = 20)
    histdat <- data.frame(x = h[["mids"]], freq = h[["counts"]],
                          pct = h[["counts"]]/sum(h[["counts"]]))
    if (is.null(ymax.l)) {ymax.l<-max(histdat$pct)+max(histdat$pct)*0.8} else {ymax.l<-ymax.l}
    sca<-ymax.l/ymaxupper
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
    if (!missing(histbinwidth)) {histbinwidth<-histbinwidth}
    if (missing(histcol)) {histcol<-"plum1"} else {assign("histcol",histcol)}
    if (missing(linetype)) {linetype<-1} else {assign("linetype",linetype)}
    if (missing(linesize)) {linesize<-1} else {assign("linesize",linesize)}
    if (missing(ribalpha)) {ribalpha<-0.3} else {assign("ribalpha",ribalpha)}
    if (missing(ribcol)) {ribcol<-"lightblue3"} else {assign("ribcol",ribcol)}
    if (missing(xlab)) {xlab<-x.name} else {assign("xlab",xlab)}
    if (missing(ylab)) {ylab<-"Outcome Prediction Incidence"} else {assign("ylab",ylab)}
    if (missing(histalpha)) {histalpha<-0.5} else {assign("histalpha",histalpha)}
    if (missing(linealpha)) {linealpha<-0.7} else {assign("linealpha",linealpha)}
    if (missing(linecol)) {linecol<-ribcol} else {linecol<-linecol}
    if (missing(leftaxislimit)) {leftaxislimit<-ymax.l} else {assign("leftaxislimit",leftaxislimit)}
    if (missing(title)) {title<-"The relationship between the variable and the predicted probability"} else {assign("title",title)}
    #############
    #browser()
    if (pdensity==FALSE) {
      p<-ggplot2::ggplot() +
        geom_bar(
          data=histdat, aes(x=x, y=pct/sca),
          stat="identity", width=(xmax-xmin)/(length(hist.breaks)-1),
          fill=histcol, color="black"
        )
    } else {
      d<-density(x)
      dmax<-as.numeric(max(d[["y"]]))##density
      #sca <- (dmax+0.7*dmax) / ymaxupper
      sca<-ymax.l/ymaxupper
      p<-ggplot2::ggplot() +
        geom_density(
          data=histdat, aes(x=x,y = after_stat(density)/sca),
          fill=histcol,color="black"
        )
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
    if (!missing(x.limits)) x.limits<-x.limits
    if (!missing(breaks) & !missing(x.limits)) {
      p<-p+scale_x_continuous(breaks = breaks, limits = x.limits)
    } else if (!missing(breaks) ) {
      p<-p+scale_x_continuous(breaks = breaks)
    } else if (!missing(x.limits)) {
      p<-p+scale_x_continuous(limits = x.limits)
    }
    ###
    if (missing(riblinecol)) {
      p<-p+geom_line(data=pre0,aes(x,yhat),linetype=linetype,linewidth=linesize,alpha = linealpha,colour=linecol)+
        geom_ribbon(data=pre0,aes(x=x,ymin = lower, ymax = upper),alpha = ribalpha,fill=ribcol)
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
    p<-p+scale_y_continuous(sec.axis = sec_axis(name = liftname,trans=~. * sca))
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
    if (missing(px)) {
      #px<-max(x)*0.3
      px<-(max(x)-min(x))*0.02+min(x)
      } else {assign("px",px)}
    if (missing(py)) {py<-max(pre0$upper)*0.95} else {assign("py",py)}
    px<-round(px)
    py<-round(py)
    #px<-max(x)*0.3
    #py<-max(pre0$upper)*0.95
    p<-p+draw_label(text, size = fontsize,
                    fontfamily = fontfamily, x = px, y = py, hjust = 0,
                    vjust = 1)
  }
  if (!missing(y.limits)) {p<-p+scale_y_continuous(limits = y.limits)}
  p
}
