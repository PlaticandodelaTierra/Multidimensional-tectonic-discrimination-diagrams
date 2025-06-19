rm(list=ls())  #borra todas las variables que esten asignados
graphics.off() #cierra todas las graficas 
##### Diagramas de discriminaci?n tect?nica log ###
readline('Welcome ! lets generate some diagrams (please push enter to continue)')
Di=readline('Please type Verma_Agrawal_2011')
if(Di=='Verma_Agrawal_2011'){ ## Basic and ultrabasic rocks
  A=readline('Did you want the diagrams all together? Yes=(Y),No=(N)')
  if (substr(A,1,1)=='Y'){
    x11()
    par(mfrow=c(2,3))
    ##### Datos y funciones ##
    D=read.csv('Verma_Agrawal_2011.csv')
    Nb=(D[,3]*0.0001)/1 
    V=(D[,4]*0.0001)/1
    Y=(D[,5]*0.0001)/1
    Zr=(D[,6]*0.0001)/1
    ## Relaciones Logaritmicas
    Nb_Ti=log(Nb/D[,7])
    V_Ti=log(V/D[,7])
    Y_Ti=log(Y/D[,7])
    Zr_Ti=log(Zr/D[,7])
    ## Funcion de IAB-CRB+OIB-MORB
    DF1_B1=(-0.6611*Nb_Ti)+(2.2926*V_Ti)+(1.6774*Y_Ti)+(1.0916*Zr_Ti)+21.3603
    DF2_B1=(0.4702*Nb_Ti)+(3.7649*V_Ti)+(-3.911*Y_Ti)+(2.2697*Zr_Ti)+4.8487
    ## Funcion de IAB-CRB-OIB
    DF1_B2=(-0.6146*Nb_Ti)+(2.3510*V_Ti)+(1.6828*Y_Ti)+(1.1911*Zr_Ti)+22.7253
    DF2_B2=(1.3765*Nb_Ti)+(-0.9452*V_Ti)+(4.0461*Y_Ti)+(-2.0789*Zr_Ti)+22.2450
    ## Funcion de IAB-CRB-MORB
    DF1_B3=(-0.6624*Nb_Ti)+(2.4498*V_Ti)+(1.2867*Y_Ti)+(1.0920*Zr_Ti)+18.7466
    DF2_B3=(0.4938*Nb_Ti)+(3.4741*V_Ti)+(-3.8053*Y_Ti)+(2.0070*Zr_Ti)+3.3163
    ## Funcion de IAB-OIB-MORB
    DF1_B4=(-0.2646*Nb_Ti)+(2.0491*V_Ti)+(3.4565*Y_Ti)+(0.8573*Zr_Ti)+32.9472
    DF2_B4=(0.01874*Nb_Ti)+(4.0937*V_Ti)+(-4.8550*Y_Ti)+(2.9900*Zr_Ti)+0.1995
    ## Funcion de CRB-OIB-MORB
    DF1_B5=(-0.7829*Nb_Ti)+(0.3379*V_Ti)+(3.3239*Y_Ti)+(0.51232*Zr_Ti)+16.0941
    DF2_B5=(1.7478*Nb_Ti)+(-0.0421*V_Ti)+(3.5301*Y_Ti)+(-1.4503*Zr_Ti)+28.3592
    #### IAB-CRB+OIB-MORB ###
    x1=c(0.02820,0.63849,-3.2318,0.63849,8,0.63849)
    y1=c(8,0.87812,-8,0.87812,-4.5532,0.87812)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB+OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB+OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,-5,'CRB(94%)+OIB(99%)')
    text(2,-5,'MORB (93%)')
    text(6,5,'IAB(88%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB ###
    x2=c(2.27820,0.883172,-8,0.883172,1.87600,0.883172)
    y2=c(8,-0.667465,1.66740,-0.667465,-8,-0.667465)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-4,6,'CRB(77%)')
    text(4,6,'IAB(99%)')
    text(-4,-5,'OIB(86%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x2,y2,lwd=2);
    #### IAB-CRB-MORB ###
    x3=c(-0.43580,-0.016496,8,-0.016496,-4.19440,-0.016496)
    y3=c(8,0.972583,-5.79920,0.972583,-8,0.972583)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,4,'CRB(99%)')
    text(3,7,'IAB(88%)')
    text(0,-5,'MORB(93%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x3,y3,lwd=2);
    #### IAB-OIB-MORB ###
    x4=c(-0.81840,-0.322489,8,-0.322489,-3.72100,-0.322489)
    y4=c(8,1.040295,-4.365,1.040295,-8,1.040295)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-6,5,'OIB(98%)')
    text(2,7,'IAB(88%)')
    text(0,-5,'MORB(96%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x4,y4,lwd=2);
    #### CRB-OIB-MORB ###
    x5=c(-8,1.129586,3.3210,1.129586,2.43,1.129586)
    y5=c(1.485,-0.4194316,8,-0.4194316,-8,-0.4194316)
    plot(DF1_B5,DF2_B5,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-4,5,'CRB(73%)')
    text(5,5,'(MORB(95%)')
    text(-4,-5,'(OIB(84%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x5,y5,lwd=2);
  }else if (A=='N'){
    ##### Datos y funciones ##
    D=read.csv('Verma_Agrawal_2011.csv')
    Nb=(D[,3]*0.0001)/1 
    V=(D[,4]*0.0001)/1
    Y=(D[,5]*0.0001)/1
    Zr=(D[,6]*0.0001)/1
    ## Relaciones Logaritmicas
    Nb_Ti=log(Nb/D[,7])
    V_Ti=log(V/D[,7])
    Y_Ti=log(Y/D[,7])
    Zr_Ti=log(Zr/D[,7])
    ## Funcion de IAB-CRB+OIB-MORB
    DF1_B1=(-0.6611*Nb_Ti)+(2.2926*V_Ti)+(1.6774*Y_Ti)+(1.0916*Zr_Ti)+21.3603
    DF2_B1=(0.4702*Nb_Ti)+(3.7649*V_Ti)+(-3.911*Y_Ti)+(2.2697*Zr_Ti)+4.8487
    ## Funcion de IAB-CRB-OIB
    DF1_B2=(-0.6146*Nb_Ti)+(2.3510*V_Ti)+(1.6828*Y_Ti)+(1.1911*Zr_Ti)+22.7253
    DF2_B2=(1.3765*Nb_Ti)+(-0.9452*V_Ti)+(4.0461*Y_Ti)+(-2.0789*Zr_Ti)+22.2450
    ## Funcion de IAB-CRB-MORB
    DF1_B3=(-0.6624*Nb_Ti)+(2.4498*V_Ti)+(1.2867*Y_Ti)+(1.0920*Zr_Ti)+18.7466
    DF2_B3=(0.4938*Nb_Ti)+(3.4741*V_Ti)+(-3.8053*Y_Ti)+(2.0070*Zr_Ti)+3.3163
    ## Funcion de IAB-OIB-MORB
    DF1_B4=(-0.2646*Nb_Ti)+(2.0491*V_Ti)+(3.4565*Y_Ti)+(0.8573*Zr_Ti)+32.9472
    DF2_B4=(0.01874*Nb_Ti)+(4.0937*V_Ti)+(-4.8550*Y_Ti)+(2.9900*Zr_Ti)+0.1995
    ## Funcion de CRB-OIB-MORB
    DF1_B5=(-0.7829*Nb_Ti)+(0.3379*V_Ti)+(3.3239*Y_Ti)+(0.51232*Zr_Ti)+16.0941
    DF2_B5=(1.7478*Nb_Ti)+(-0.0421*V_Ti)+(3.5301*Y_Ti)+(-1.4503*Zr_Ti)+28.3592
    #### IAB-CRB+OIB-MORB ###
    x1=c(0.02820,0.63849,-3.2318,0.63849,8,0.63849)
    y1=c(8,0.87812,-8,0.87812,-4.5532,0.87812)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB+OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB+OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,-5,'CRB(94%)+OIB(99%)')
    text(2,-5,'MORB (93%)')
    text(6,5,'IAB(88%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB ###
    x2=c(2.27820,0.883172,-8,0.883172,1.87600,0.883172)
    y2=c(8,-0.667465,1.66740,-0.667465,-8,-0.667465)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-4,6,'CRB(77%)')
    text(4,6,'IAB(99%)')
    text(-4,-5,'OIB(86%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x2,y2,lwd=2);
    #### IAB-CRB-MORB ###
    x3=c(-0.43580,-0.016496,8,-0.016496,-4.19440,-0.016496)
    y3=c(8,0.972583,-5.79920,0.972583,-8,0.972583)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,4,'CRB(99%)')
    text(3,7,'IAB(88%)')
    text(0,-5,'MORB(93%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x3,y3,lwd=2);
    #### IAB-OIB-MORB ###
    x4=c(-0.81840,-0.322489,8,-0.322489,-3.72100,-0.322489)
    y4=c(8,1.040295,-4.365,1.040295,-8,1.040295)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-6,5,'OIB(98%)')
    text(2,7,'IAB(88%)')
    text(0,-5,'MORB(96%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x4,y4,lwd=2);
    #### CRB-OIB-MORB ###
    x5=c(-8,1.129586,3.3210,1.129586,2.43,1.129586)
    y5=c(1.485,-0.4194316,8,-0.4194316,-8,-0.4194316)
    plot(DF1_B5,DF2_B5,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-4,5,'CRB(73%)')
    text(5,5,'(MORB(95%)')
    text(-4,-5,'(OIB(84%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x5,y5,lwd=2);
  }
  B=readline('Did you want to generate a report with the DF and log ratios values? Yes=(Y),No=(N)')
  if (B=='Y'){
    X=D[,2]
    x=D[,1] ### Colocar la posici?n de la muestra
    fa=data.frame(D,X,Nb,V,Y,Zr,X,Nb_Ti,V_Ti,Y_Ti,Zr_Ti,X,DF1_B1,DF2_B1,DF1_B2,DF2_B2,DF1_B3,DF2_B3,DF1_B4,DF2_B4,DF1_B5,DF2_B5)
    write.csv(fa,file='Report_Verma_Agrawal_2011.csv',row.names=T)
  }else if (B=='N'){
    b=print('Thanks, I invite you to generate more diagrams at www.platicandodelatierra.com ')
  }
}