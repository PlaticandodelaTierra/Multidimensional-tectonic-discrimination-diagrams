rm(list=ls())  #borra todas las variables que esten asignados
graphics.off() #cierra todas las graficas 
##### Diagramas de discriminaci?n tect?nica log ###
readline('Welcome ! lets generate some diagrams (please push enter to continue)')
Di=readline('Please type Agrawal_etal_2004')
if(Di=='Agrawal_etal_2004'){ ## Basic Rocks
  A=readline('Did you want the diagrams all together? Yes=(Y),No=(N)')
  if (substr(A,1,1)=='Y'){
    x11()
    par(mfrow=c(2,3))
    D=read.csv('Agrawal_etal_2004.csv')
    ## Funcion de IAB-CRB-OIB-MORB
    DF1_B1=(0.258*D[1:5,3])+(2.395*D[1:5,4])+(0.106*D[1:5,5])+(1.019*D[1:5,6])+(-6.778*D[1:5,8])+(0.405*D[1:5,9])+(0.119*D[1:5,10])+(0.071*D[1:5,11])+(-0.198*D[1:5,12])+(0.613*D[1:5,13])-24.065
    DF2_B1=(.730*D[1:5,3])+(1.119*D[1:5,4])+(0.156*D[1:5,5])+(1.332*D[1:5,6])+(4.376*D[1:5,8])+(0.493*D[1:5,9])+(0.936*D[1:5,10])+(0.882*D[1:5,11])+(-0.291*D[1:5,12])+(-1.572*D[1:5,13])-59.472
    ## Funcion de IAB-CRB-OIB 
    DF1_B2=(0.251*D[1:5,3])+(2.034*D[1:5,4])+(-0.1*D[1:5,5])+(0.573*D[1:5,6])+(0.032*D[1:5,7])+(-2.877*D[1:5,8])+(0.260*D[1:5,9])+(0.052*D[1:5,10])+(0.322*D[1:5,11])+(-0.229*D[1:5,12])-18.974
    DF2_B2=(2.150*D[1:5,3])+(2.711*D[1:5,4])+(1.792*D[1:5,5])+(2.295*D[1:5,6])+(1.484*D[1:5,7])+(8.594*D[1:5,8])+(1.896*D[1:5,9])+(2.158*D[1:5,10])+(1.201*D[1:5,11])+(1.763*D[1:5,12])-200.276
    ## Funcion de IAB-CRB-MORB    
    DF1_B3=(0.435*D[1:5,3])+(-1.392*D[1:5,4])+(0.183*D[1:5,5])+(0.148*D[1:5,7])+(7.690*D[1:5,8])+(0.021*D[1:5,9])+(0.380*D[1:5,10])+(0.036*D[1:5,11])+(0.462*D[1:5,12])+(-1.192*D[1:5,13])-29.435
    DF2_B3=(0.601*D[1:5,3])+(-0.335*D[1:5,4])+(1.332*D[1:5,5])+(1.449*D[1:5,7])+(0.756*D[1:5,8])+(0.893*D[1:5,9])+(0.448*D[1:5,10])+(0.525*D[1:5,11])+(1.734*D[1:5,12])+(2.494*D[1:5,13])-78.236
    ## Funcion de IAB-OIB-MORB      
    DF1_B4=(1.232*D[1:5,3])+(4.166*D[1:5,4])+(1.085*D[1:5,5])+(3.522*D[1:5,6])+(0.500*D[1:5,7])+(-3.930*D[1:5,8])+(1.334*D[1:5,9])+(1.085*D[1:5,10])+(0.416*D[1:5,11])+(0.827*D[1:5,12])-119.050
    DF2_B4=(1.384*D[1:5,3])+(1.091*D[1:5,4])+(0.908*D[1:5,5])+(2.419*D[1:5,6])+(0.886*D[1:5,7])+(5.281*D[1:5,8])+(1.269*D[1:5,9])+(1.790*D[1:5,10])+(2.572*D[1:5,11])+(0.138*D[1:5,12])-134.295
    ## Funcion de CRB-OIB-MORB   
    DF1_B5=(0.310*D[1:5,3])+(1.936*D[1:5,4])+(0.341*D[1:5,5])+(0.760*D[1:5,6])+(0.351*D[1:5,7])+(-11.315*D[1:5,8])+(0.526*D[1:5,9])+(0.084*D[1:5,10])+(0.312*D[1:5,12])+(1.892*D[1:5,13])-32.909
    DF2_B5=(0.703*D[1:5,3])+(2.454*D[1:5,4])+(0.233*D[1:5,5])+(1.943*D[1:5,6])+(-0.182*D[1:5,7])+(-2.421*D[1:5,8])+(0.618*D[1:5,9])+(0.712*D[1:5,10])+(-0.866*D[1:5,12])+(-1.180*D[1:5,13])-56.455
    #### IAB-CRB-OIB-MORB ###
    x1=c(-1.03,-0.52,0.80,8,0.80,2.67,0.80,-0.52,-8);
    y1=c(-8,-0.99,0.32,-4.75,0.32,8,0.32,-1.03,6.33);
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,-2,'IAB (84%)')
    text(2,-5,'CRB (80%)')
    text(5,3,'OIB (88%)')
    text(-2,4,'MORB (92%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB-MORB ###
    x1=c(-0.52,-2.76,-0.52,8,-0.52,-1.09)
    y1=c(1.34,-8,1.34,-5.11,1.34,8)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,3,'IAB (92%)')
    text(2,-5,'CRB (88%)')
    text(5,3,'OIB (84%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ####IAB-CRB-MORB ###
    x1=c(-0.49,-4.97,-0.49,8,-0.49,-1.93)
    y1=c(0.84,-8,0.84,-3.04,0.84,8)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,4,'IAB (88%)')
    text(-5,5,'CRB (96%)')
    text(2,-4,'MORB (92%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IAB-OIB-MORB ###
    x1=c(0.50,0.97,0.50,4.27,0.50,-8)
    y1=c(-2.17,-8,-2.17,8,-2.17,7.10)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,0,'IAB (92%)')
    text(-1,5,'MORB (92%)')
    text(5,0,'OIB (96%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CRB-OIB-MORB ###
    x1=c(0.17,-3.83,0.17,8,0.17,-2.28)
    y1=c(0.07,-8,0.07,-1.17,0.07,8)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,2,'MORB (96%)')
    text(4,4,'OIB (88%)')
    text(2,-4,'CRB (76%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }else if (A=='N'){
    D=read.csv('Agrawal_etal_2004.csv')
    ## Funcion de IAB-CRB-OIB-MORB
    DF1_B1=(0.258*D[1:5,3])+(2.395*D[1:5,4])+(0.106*D[1:5,5])+(1.019*D[1:5,6])+(-6.778*D[1:5,8])+(0.405*D[1:5,9])+(0.119*D[1:5,10])+(0.071*D[1:5,11])+(-0.198*D[1:5,12])+(0.613*D[1:5,13])-24.065
    DF2_B1=(.730*D[1:5,3])+(1.119*D[1:5,4])+(0.156*D[1:5,5])+(1.332*D[1:5,6])+(4.376*D[1:5,8])+(0.493*D[1:5,9])+(0.936*D[1:5,10])+(0.882*D[1:5,11])+(-0.291*D[1:5,12])+(-1.572*D[1:5,13])-59.472
    ## Funcion de IAB-CRB-OIB 
    DF1_B2=(0.251*D[1:5,3])+(2.034*D[1:5,4])+(-0.1*D[1:5,5])+(0.573*D[1:5,6])+(0.032*D[1:5,7])+(-2.877*D[1:5,8])+(0.260*D[1:5,9])+(0.052*D[1:5,10])+(0.322*D[1:5,11])+(-0.229*D[1:5,12])-18.974
    DF2_B2=(2.150*D[1:5,3])+(2.711*D[1:5,4])+(1.792*D[1:5,5])+(2.295*D[1:5,6])+(1.484*D[1:5,7])+(8.594*D[1:5,8])+(1.896*D[1:5,9])+(2.158*D[1:5,10])+(1.201*D[1:5,11])+(1.763*D[1:5,12])-200.276
    ## Funcion de IAB-CRB-MORB    
    DF1_B3=(0.435*D[1:5,3])+(-1.392*D[1:5,4])+(0.183*D[1:5,5])+(0.148*D[1:5,7])+(7.690*D[1:5,8])+(0.021*D[1:5,9])+(0.380*D[1:5,10])+(0.036*D[1:5,11])+(0.462*D[1:5,12])+(-1.192*D[1:5,13])-29.435
    DF2_B3=(0.601*D[1:5,3])+(-0.335*D[1:5,4])+(1.332*D[1:5,5])+(1.449*D[1:5,7])+(0.756*D[1:5,8])+(0.893*D[1:5,9])+(0.448*D[1:5,10])+(0.525*D[1:5,11])+(1.734*D[1:5,12])+(2.494*D[1:5,13])-78.236
    ## Funcion de IAB-OIB-MORB      
    DF1_B4=(1.232*D[1:5,3])+(4.166*D[1:5,4])+(1.085*D[1:5,5])+(3.522*D[1:5,6])+(0.500*D[1:5,7])+(-3.930*D[1:5,8])+(1.334*D[1:5,9])+(1.085*D[1:5,10])+(0.416*D[1:5,11])+(0.827*D[1:5,12])-119.050
    DF2_B4=(1.384*D[1:5,3])+(1.091*D[1:5,4])+(0.908*D[1:5,5])+(2.419*D[1:5,6])+(0.886*D[1:5,7])+(5.281*D[1:5,8])+(1.269*D[1:5,9])+(1.790*D[1:5,10])+(2.572*D[1:5,11])+(0.138*D[1:5,12])-134.295
    ## Funcion de CRB-OIB-MORB   
    DF1_B5=(0.310*D[1:5,3])+(1.936*D[1:5,4])+(0.341*D[1:5,5])+(0.760*D[1:5,6])+(0.351*D[1:5,7])+(-11.315*D[1:5,8])+(0.526*D[1:5,9])+(0.084*D[1:5,10])+(0.312*D[1:5,12])+(1.892*D[1:5,13])-32.909
    DF2_B5=(0.703*D[1:5,3])+(2.454*D[1:5,4])+(0.233*D[1:5,5])+(1.943*D[1:5,6])+(-0.182*D[1:5,7])+(-2.421*D[1:5,8])+(0.618*D[1:5,9])+(0.712*D[1:5,10])+(-0.866*D[1:5,12])+(-1.180*D[1:5,13])-56.455
    #### IAB-CRB-OIB-MORB ###
    x1=c(-1.03,-0.52,0.80,8,0.80,2.67,0.80,-0.52,-8);
    y1=c(-8,-0.99,0.32,-4.75,0.32,8,0.32,-1.03,6.33);
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,-2,'IAB (84%)')
    text(2,-5,'CRB (80%)')
    text(5,3,'OIB (88%)')
    text(-2,4,'MORB (92%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB-MORB ###
    x1=c(-0.52,-2.76,-0.52,8,-0.52,-1.09)
    y1=c(1.34,-8,1.34,-5.11,1.34,8)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,3,'IAB (92%)')
    text(2,-5,'CRB (88%)')
    text(5,3,'OIB (84%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ####IAB-CRB-MORB ###
    x1=c(-0.49,-4.97,-0.49,8,-0.49,-1.93)
    y1=c(0.84,-8,0.84,-3.04,0.84,8)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,4,'IAB (88%)')
    text(-5,5,'CRB (96%)')
    text(2,-4,'MORB (92%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IAB-OIB-MORB ###
    x1=c(0.50,0.97,0.50,4.27,0.50,-8)
    y1=c(-2.17,-8,-2.17,8,-2.17,7.10)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,0,'IAB (92%)')
    text(-1,5,'MORB (92%)')
    text(5,0,'OIB (96%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CRB-OIB-MORB ###
    x1=c(0.17,-3.83,0.17,8,0.17,-2.28)
    y1=c(0.07,-8,0.07,-1.17,0.07,8)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,2,'MORB (96%)')
    text(4,4,'OIB (88%)')
    text(2,-4,'CRB (76%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }
  B=readline('Did you want to generate a report with the DF values? Yes=(Y),No=(N)')
  if (B=='Y'){
    X=D[,2]
    x=D[,1] ### Colocar la posici?n de la muestra
    fa=data.frame(D,X,DF1_B1,DF2_B1,DF1_B2,DF2_B2,DF1_B3,DF2_B3,DF1_B4,DF2_B4,DF1_B5,DF2_B5)
    write.csv(fa,file='Report_Agrawal_2004.csv',row.names=T)
  }else if (B=='N'){
    b=print('Thanks, I invite you to generate more diagrams at www.platicandodelatierra.com ')
  }
}