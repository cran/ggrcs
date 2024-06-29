## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggrcs)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(rms)
library(ggplot2)
library(scales)
library(ggrcs)
library(cowplot)

## ----paged.print=FALSE--------------------------------------------------------
dt<-smoke

## -----------------------------------------------------------------------------
dd<-datadist(dt)
options(datadist='dd')

## -----------------------------------------------------------------------------
fit<- cph(Surv(time,status==1) ~ rcs(age,4)+gender, x=TRUE, y=TRUE,data=dt)

## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age")

## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",histcol="blue",histbinwidth=1)

## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",histcol="blue",histbinwidth=1,ribcol="green",ribalpha=0.5)

## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",histcol="blue",
      histbinwidth=1,ribcol="green",ribalpha=0.5,xlab ="age(year)",
      ylab="disease prevalence",title ="Relationship between age and disease prevalence.",liftname="Probability density")

## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",histcol="blue",
      histbinwidth=1,ribcol="green",ribalpha=0.5,xlab ="age(year)",
      ylab="disease prevalence",title ="Relationship between age and disease prevalence.",liftname="Probability density",lift=F,P.Nonlinear=T)


## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",histcol="blue",
      histbinwidth=1,ribcol="green",ribalpha=0.5,xlab ="age(year)",
      ylab="disease prevalence",title ="Relationship between age and disease prevalence.",liftname="Probability density",lift=F,P.Nonlinear=T,Pvalue="<0.05")

## -----------------------------------------------------------------------------
p<-ggrcs(data=dt,fit=fit,x="age",histcol="blue",
      histbinwidth=1,ribcol="green",ribalpha=0.5,xlab ="age(year)",
      ylab="disease prevalence",title ="Relationship between age and disease prevalence.",liftname="Probability density",lift=F,P.Nonlinear=T,Pvalue="<0.05")
p+geom_vline(aes(xintercept=45.6),colour="red", linetype=1)

## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",group="gender")


## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",group="gender",groupcol=c("red","blue"),histbinwidth=1)


## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",group="gender",
      groupcol=c("red","blue"),histbinwidth=1,ribalpha=0.5,xlab ="age(year)",
      ylab="disease prevalence",title ="Relationship between age and disease prevalence.",leftaxislimit=c(0,1),liftname="Probability density",
      P.Nonlinear=T,px=25,py=18)


## -----------------------------------------------------------------------------
ggrcs(data=dt,fit=fit,x="age",group="gender",
      groupcol=c("red","blue"),histbinwidth=1,ribalpha=0.5,xlab ="age(year)",
      ylab="disease prevalence",title ="Relationship between age and disease prevalence.",leftaxislimit=c(0,1),liftname="Probability density",
      P.Nonlinear=T,Pvalue="<0.05",px=25,py=18,twotag.name= c("m","f"))

## -----------------------------------------------------------------------------
singlercs(data=dt,fit=fit,x="age")

## -----------------------------------------------------------------------------
p<-singlercs(data=dt,fit=fit,x="age")
p+geom_hline(yintercept=1, linetype=2,linewidth=1)

## -----------------------------------------------------------------------------
singlercs(data=dt,fit=fit,x="age",ribcol="green")

## -----------------------------------------------------------------------------
singlercs(data=dt,fit=fit,x="age",ribcol="green",ribalpha=0.2)

## -----------------------------------------------------------------------------
singlercs(data=dt,fit=fit,x="age",group="gender")


## -----------------------------------------------------------------------------
singlercs(data=dt,fit=fit,x="age",group="gender",groupcol=c("red","blue"))

