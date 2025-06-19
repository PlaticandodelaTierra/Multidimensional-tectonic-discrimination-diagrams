rm(list=ls())  #borra todas las variables que esten asignados
graphics.off() #cierra todas las graficas 
##### Diagramas de discriminaci?n tect?nica log ###
readline('Welcome ! lets generate some diagrams (please push enter to continue)')
Di=readline('Please type Agrawal_etal_2008')
if(Di=='Agrawal_etal_2008'){ #### Basic and ultrabasic rocks
  A=readline('Did you want the diagrams all together? Yes=(Y),No=(N)')
  if (substr(A,1,1)=='Y'){
    x11()
    par(mfrow=c(2,3))
    ##### Datos y funciones ###
    D=read.csv('Agrawal_etal_2008.csv')
    ## Relaciones Logaritmicas
    La_Th=log(D[,3]/D[,4]) #v16
    Sm_Th=log(D[,5]/D[,4]) #v17
    Yb_Th=log(D[,6]/D[,4]) #v18
    Nb_Th=log(D[,7]/D[,4]) #v19
    ## Funcion de IAB-CRB+OIB-MORB
    DF1_B1=(.3518*La_Th)+(.6013 *Sm_Th)+(-1.3450*Yb_Th)+(2.1056*Nb_Th)-5.4763
    DF2_B1=(-.3050*La_Th)+(-1.1801*Sm_Th)+(1.6189*Yb_Th)+(1.2260 *Nb_Th)-0.9944
    ## Funcion de IAB-CRB-OIB 
    DF1_B2=(0.5533 *La_Th)+(.2173 *Sm_Th)+(-.0969* Yb_Th)+(2.0454*Nb_Th)-5.6305
    DF2_B2=(-2.4498 *La_Th)+( 4.8562 *Sm_Th)+(-2.1240 *Yb_Th)-(0.1567 *Nb_Th)+ .94
    ## Funcion de IAB-CRB-MORB    
    DF1_B3=(0.3305*La_Th)+(0.3484 *Sm_Th)+(-0.9562* Yb_Th)+(2.0777*Nb_Th) -4.5628
    DF2_B3=(-0.1928*La_Th)+(-1.1989*Sm_Th)+(1.7531 *Yb_Th)+(0.6607 *Nb_Th)-0.4384
    ## Funcion de IAB-OIB-MORB      
    DF1_B4=(1.7517*Sm_Th)+(-1.9508 *Yb_Th)+(1.9573 *Nb_Th)-5.0928
    DF2_B4=(-2.2412*Sm_Th)+(2.2060 *Yb_Th)+(1.2481 *Nb_Th)-0.8243
    ## Funcion de CRB-OIB-MORB   
    DF1_B5=(-0.558*La_Th)+(-1.4260 *Sm_Th)+(2.2935 *Yb_Th)-(0.6890 *Nb_Th)+ 4.1422
    DF2_B5=(-0.9207*La_Th)+(3.6520 *Sm_Th)-(1.9866 *Yb_Th)+(1.0574*Nb_Th)-4.4283
    ### IAB-CRB+OIB-MORB ###
    x1=c(-0.12,-1.03,8,-1.03,-8,-1.03);
    y1=c(-8,-0.47,7.32,-0.47,5.6,-0.47);
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB+OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(0,5,'MORB(96%)')
    text(-5,-4,'IAB(91%)')
    text(4,0,'CRB+OIB(95%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB ###
    x1=c(-1.12,-2.34,-1.12,8,-1.12,-1.8)
    y1=c(0.71,-8,0.71,-2.08,0.71,8)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(3,5,'OIB(85%)')
    text(-5,4,'IAB(95%)')
    text(0,-5,'CRB(78%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ####IAB-CRB-MORB ###
    x1=c(-0.44,-1.22,-0.44,8,-0.44,-7.18)
    y1=c(-0.39,-8,-0.39,3.65,-0.39,8)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(0,5,'MORB(96%)')
    text(-5,-2,'IAB(91%)')
    text(5,-3,'CRB(92%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IAB-OIB-MORB ###
    x1=c(-0.36,0.03,-0.36,8,-0.36,-8)
    y1=c(-0.78,-8,-0.78,7.03,-0.78,6.06)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,0,'OIB(98%)')
    text(-5,-4,'IAB(92%)')
    text(0,5,'MORB(97%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CRB-OIB-MORB ###
    x1=c(0.67,-8,0.67,1.58,0.67,2.31)
    y1=c(0.48,-2.06,0.48,8,0.48,-8)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,4,'OIB(77%)')
    text(-5,-4,'CRB(70%)')
    text(5,0,'MORB(94%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }else if (A=='N'){
    ##### Datos y funciones ###
    D=read.csv('Agrawal_etal_2008.csv')
    ## Relaciones Logaritmicas
    La_Th=log(D[,3]/D[,4]) #v16
    Sm_Th=log(D[,5]/D[,4]) #v17
    Yb_Th=log(D[,6]/D[,4]) #v18
    Nb_Th=log(D[,7]/D[,4]) #v19
    ## Funcion de IAB-CRB+OIB-MORB
    DF1_B1=(.3518*La_Th)+(.6013 *Sm_Th)+(-1.3450*Yb_Th)+(2.1056*Nb_Th)-5.4763
    DF2_B1=(-.3050*La_Th)+(-1.1801*Sm_Th)+(1.6189*Yb_Th)+(1.2260 *Nb_Th)-0.9944
    ## Funcion de IAB-CRB-OIB 
    DF1_B2=(0.5533 *La_Th)+(.2173 *Sm_Th)+(-.0969* Yb_Th)+(2.0454*Nb_Th)-5.6305
    DF2_B2=(-2.4498 *La_Th)+( 4.8562 *Sm_Th)+(-2.1240 *Yb_Th)-(0.1567 *Nb_Th)+ .94
    ## Funcion de IAB-CRB-MORB    
    DF1_B3=(0.3305*La_Th)+(0.3484 *Sm_Th)+(-0.9562* Yb_Th)+(2.0777*Nb_Th) -4.5628
    DF2_B3=(-0.1928*La_Th)+(-1.1989*Sm_Th)+(1.7531 *Yb_Th)+(0.6607 *Nb_Th)-0.4384
    ## Funcion de IAB-OIB-MORB      
    DF1_B4=(1.7517*Sm_Th)+(-1.9508 *Yb_Th)+(1.9573 *Nb_Th)-5.0928
    DF2_B4=(-2.2412*Sm_Th)+(2.2060 *Yb_Th)+(1.2481 *Nb_Th)-0.8243
    ## Funcion de CRB-OIB-MORB   
    DF1_B5=(-0.558*La_Th)+(-1.4260 *Sm_Th)+(2.2935 *Yb_Th)-(0.6890 *Nb_Th)+ 4.1422
    DF2_B5=(-0.9207*La_Th)+(3.6520 *Sm_Th)-(1.9866 *Yb_Th)+(1.0574*Nb_Th)-4.4283
    ### IAB-CRB+OIB-MORB ###
    x1=c(-0.12,-1.03,8,-1.03,-8,-1.03);
    y1=c(-8,-0.47,7.32,-0.47,5.6,-0.47);
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB+OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(0,5,'MORB(96%)')
    text(-5,-4,'IAB(91%)')
    text(4,0,'CRB+OIB(95%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB ###
    x1=c(-1.12,-2.34,-1.12,8,-1.12,-1.8)
    y1=c(0.71,-8,0.71,-2.08,0.71,8)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(3,5,'OIB(85%)')
    text(-5,4,'IAB(95%)')
    text(0,-5,'CRB(78%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ####IAB-CRB-MORB ###
    x1=c(-0.44,-1.22,-0.44,8,-0.44,-7.18)
    y1=c(-0.39,-8,-0.39,3.65,-0.39,8)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(0,5,'MORB(96%)')
    text(-5,-2,'IAB(91%)')
    text(5,-3,'CRB(92%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IAB-OIB-MORB ###
    x1=c(-0.36,0.03,-0.36,8,-0.36,-8)
    y1=c(-0.78,-8,-0.78,7.03,-0.78,6.06)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,0,'OIB(98%)')
    text(-5,-4,'IAB(92%)')
    text(0,5,'MORB(97%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CRB-OIB-MORB ###
    x1=c(0.67,-8,0.67,1.58,0.67,2.31)
    y1=c(0.48,-2.06,0.48,8,0.48,-8)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,4,'OIB(77%)')
    text(-5,-4,'CRB(70%)')
    text(5,0,'MORB(94%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }
  B=readline('Did you want to generate a report with the DF and log ratios values? Yes=(Y),No=(N)')
  if (B=='Y'){
    X=D[,2]
    x=D[,1] ### Colocar la posici?n de la muestra
    fa=data.frame(D,X,La_Th,Sm_Th,Yb_Th,Nb_Th,X,DF1_B1,DF2_B1,DF1_B2,DF2_B2,DF1_B3,DF2_B3,DF1_B4,DF2_B4,DF1_B5,DF2_B5)
    write.csv(fa,file='Report_Agrawal_2008.csv',row.names=T)
  }else if (B=='N'){
    b=print('Thanks, I invite you to generate more diagrams at www.platicandodelatierra.com ')
  }
}