rm(list=ls())  #borra todas las variables que esten asignados
graphics.off() #cierra todas las graficas 
##### Diagramas de discriminaci?n tect?nica log ###
readline('Welcome ! lets generate some diagrams (please push enter to continue)')
Di=readline('Please type Verma_etal_2006')
if(Di=='Verma_etal_2006'){ ## Basic and ultrabasic rocks
  A=readline('Did you want the diagrams all together? Yes=(Y),No=(N)')
  if (substr(A,1,1)=='Y'){
    x11()
    par(mfrow=c(2,3))
    #### Datos y funciones ###
    D=read.csv('Verma_etal_2006.csv')
    ## Relaciones Logaritmicas
    Ti_Si=log(D[,4]/D[,3])
    Al_Si=log(D[,5]/D[,3])
    Fe_Si=log(D[,6]/D[,3])
    Feo_Si=log(D[,7]/D[,3])
    Mn_Si=log(D[,8]/D[,3])
    Mg_Si=log(D[,9]/D[,3])
    Ca_Si=log(D[,10]/D[,3])
    Na_Si=log(D[,11]/D[,3])
    K_Si=log(D[,12]/D[,3])
    P_Si=log(D[,13]/D[,3])
    ## Funcion de IAB-CRB-OIB-MORB
    DF1_B1=(-4.6761*Ti_Si)+(2.5330*Al_Si)+(-0.3884*Fe_Si)+(3.9688*Feo_Si)+(0.8980*Mn_Si)+(-0.5832*Mg_Si)+(-0.2896*Ca_Si)+(-0.2704*Na_Si)+(1.0810*K_Si)+(0.1845*P_Si)+1.5445
    DF2_B1=(0.6751*Ti_Si)+(4.5895*Al_Si)+(2.0897*Fe_Si)+(0.8514*Feo_Si)+(-0.4334*Mn_Si)+(1.4832*Mg_Si)+(-2.3627*Ca_Si)+(-1.6558*Na_Si)+(0.6757*K_Si)+(0.4130*P_Si)+13.1639
    ## Funcion de IAB-CRB-OIB 
    DF1_B2=(3.9998*Ti_Si)+(-2.2385*Al_Si)+(0.8110*Fe_Si)+(-2.5865*Feo_Si)+(-1.2433*Mn_Si)+(0.4872*Mg_Si)+(-0.3153*Ca_Si)+(0.4325*Na_Si)+(-1.0262*K_Si)+(0.0514*P_Si)-0.5718
    DF2_B2=(-1.3705*Ti_Si)+(3.0104*Al_Si)+(0.3239*Fe_Si)+(1.8998*Feo_Si)+(-1.9746*Mn_Si)+(1.4411*Mg_Si)+(-2.2656*Ca_Si)+(1.8665*Na_Si)+(0.2872*K_Si)+(0.8138*P_Si)+1.8202
    ## Funcion de IAB-CRB-MORB    
    DF1_B3=(-1.5736*Ti_Si)+(6.1498*Al_Si)+(1.544*Fe_Si)+(3.4134*Feo_Si)+(-0.0087*Mn_Si)+(1.2480*Mg_Si)+(-2.1103*Ca_Si)+(-0.7576*Na_Si)+(1.1431*K_Si)+(0.3524*P_Si)+16.8712
    DF2_B3=(3.9844*Ti_Si)+(0.2200*Al_Si)+(1.1516*Fe_Si)+(-2.2036*Feo_Si)+(-1.6228*Mn_Si)+(1.4291*Mg_Si)+(-1.2524*Ca_Si)+(0.3581*Na_Si)+(-0.6414*K_Si)+(0.2646*P_Si)+5.0506
    ## Funcion de IAB-OIB-MORB      
    DF1_B4=(5.3396*Ti_Si)+(-1.6279*Al_Si)+(0.8338*Fe_Si)+(-4.7362*Feo_Si)+(-0.1254*Mn_Si)+(0.6452*Mg_Si)+(1.5153*Ca_Si)+(-0.8154*Na_Si)+(-0.8888*K_Si)+(-0.2255*P_Si)+5.7755
    DF2_B4=(1.1799*Ti_Si)+(5.5114*Al_Si)+(2.7737*Fe_Si)+(-0.1341*Feo_Si)+(0.6672*Mn_Si)+(1.1045*Mg_Si)+(-1.7231*Ca_Si)+(-3.8948*Na_Si)+(0.9471*K_Si)+(-0.1082*P_Si)+15.4984
    ## Funcion de CRB-OIB-MORB   
    DF1_B5=(-0.5183*Ti_Si)+(4.9886*Al_Si)+(2.2204*Fe_Si)+(1.1801*Feo_Si)+(-0.3008*Mn_Si)+(1.3297*Mg_Si)+(-2.1834*Ca_Si)+(-1.9319*Na_Si)+(0.6976*K_Si)+(0.8998*P_Si)+13.2625
    DF2_B5=(5.0509*Ti_Si)+(-0.4972*Al_Si)+(1.0046*Fe_Si)+(-3.3848*Feo_Si)+(0.5528*Mn_Si)+(0.2925*Mg_Si)+(0.4007*Ca_Si)+(-2.8637*Na_Si)+(-0.2189*K_Si)+(-1.0558*P_Si)+2.8877
    #### IAB-CRB-OIB-MORB ###
    x1=c(3.431,1.16,1.16,1.16,5.912,1.16,-0.266,-0.266,-4.19,-0.266,-8)
    y1=c(-8,-0.333,-0.333,-0.333,8,-0.333,0.02,0.02,8,0.02,-2.49)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,2,'OIB (71%)')
    text(1,5,'CRB (84%)')
    text(6,0,'IAB (94%)')
    text(-4,-4,'MORB(97%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB ###
    x1=c(-0.733,-3.788,-0.733,8,-0.733,-1.343)
    y1=c(-1.405,8,-1.405,5.428,-1.405,-8)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,0,'IAB (94%)')
    text(0,5,'CRB (83%)')
    text(5,-3,'OIB (83%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ####IAB-CRB-MORB ###
    x1=c(8,0.361,-2.673,0.361,-6.779,0.361)
    y1=c(-1.331,-0.619,8,-0.619,-8,-0.619)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,4,'MORB (99%)')
    text(2,5,'CRB (97%)')
    text(3,-3,'IAB (94%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IAB-OIB-MORB ###
    x1=c(-0.830,-1.824,-0.830,8,-0.830,-4.865)
    y1=c(1.224,8,1.224,-3.583,1.224,-8)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,4,'IAB (99%)')
    text(0,-5,'MORB (100%)')
    text(2,5,'OIB (84%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CRB-OIB-MORB ###
    x1=c(0.029,8,0.029,-6.177,0.029,-0.819)
    y1=c(-0.222,4.322,-0.222,8,-0.222,-8)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,-2,'CRB (83%)')
    text(-5,0,'MORB (99%)')
    text(0,5,'OIB (71%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }else if (A=='N'){
    #### Datos y funciones ###
    D=read.csv('Verma_etal_2006.csv')
    ## Relaciones Logaritmicas
    Ti_Si=log(D[,4]/D[,3])
    Al_Si=log(D[,5]/D[,3])
    Fe_Si=log(D[,6]/D[,3])
    Feo_Si=log(D[,7]/D[,3])
    Mn_Si=log(D[,8]/D[,3])
    Mg_Si=log(D[,9]/D[,3])
    Ca_Si=log(D[,10]/D[,3])
    Na_Si=log(D[,11]/D[,3])
    K_Si=log(D[,12]/D[,3])
    P_Si=log(D[,13]/D[,3])
    ## Funcion de IAB-CRB-OIB-MORB
    DF1_B1=(-4.6761*Ti_Si)+(2.5330*Al_Si)+(-0.3884*Fe_Si)+(3.9688*Feo_Si)+(0.8980*Mn_Si)+(-0.5832*Mg_Si)+(-0.2896*Ca_Si)+(-0.2704*Na_Si)+(1.0810*K_Si)+(0.1845*P_Si)+1.5445
    DF2_B1=(0.6751*Ti_Si)+(4.5895*Al_Si)+(2.0897*Fe_Si)+(0.8514*Feo_Si)+(-0.4334*Mn_Si)+(1.4832*Mg_Si)+(-2.3627*Ca_Si)+(-1.6558*Na_Si)+(0.6757*K_Si)+(0.4130*P_Si)+13.1639
    ## Funcion de IAB-CRB-OIB 
    DF1_B2=(3.9998*Ti_Si)+(-2.2385*Al_Si)+(0.8110*Fe_Si)+(-2.5865*Feo_Si)+(-1.2433*Mn_Si)+(0.4872*Mg_Si)+(-0.3153*Ca_Si)+(0.4325*Na_Si)+(-1.0262*K_Si)+(0.0514*P_Si)-0.5718
    DF2_B2=(-1.3705*Ti_Si)+(3.0104*Al_Si)+(0.3239*Fe_Si)+(1.8998*Feo_Si)+(-1.9746*Mn_Si)+(1.4411*Mg_Si)+(-2.2656*Ca_Si)+(1.8665*Na_Si)+(0.2872*K_Si)+(0.8138*P_Si)+1.8202
    ## Funcion de IAB-CRB-MORB    
    DF1_B3=(-1.5736*Ti_Si)+(6.1498*Al_Si)+(1.544*Fe_Si)+(3.4134*Feo_Si)+(-0.0087*Mn_Si)+(1.2480*Mg_Si)+(-2.1103*Ca_Si)+(-0.7576*Na_Si)+(1.1431*K_Si)+(0.3524*P_Si)+16.8712
    DF2_B3=(3.9844*Ti_Si)+(0.2200*Al_Si)+(1.1516*Fe_Si)+(-2.2036*Feo_Si)+(-1.6228*Mn_Si)+(1.4291*Mg_Si)+(-1.2524*Ca_Si)+(0.3581*Na_Si)+(-0.6414*K_Si)+(0.2646*P_Si)+5.0506
    ## Funcion de IAB-OIB-MORB      
    DF1_B4=(5.3396*Ti_Si)+(-1.6279*Al_Si)+(0.8338*Fe_Si)+(-4.7362*Feo_Si)+(-0.1254*Mn_Si)+(0.6452*Mg_Si)+(1.5153*Ca_Si)+(-0.8154*Na_Si)+(-0.8888*K_Si)+(-0.2255*P_Si)+5.7755
    DF2_B4=(1.1799*Ti_Si)+(5.5114*Al_Si)+(2.7737*Fe_Si)+(-0.1341*Feo_Si)+(0.6672*Mn_Si)+(1.1045*Mg_Si)+(-1.7231*Ca_Si)+(-3.8948*Na_Si)+(0.9471*K_Si)+(-0.1082*P_Si)+15.4984
    ## Funcion de CRB-OIB-MORB   
    DF1_B5=(-0.5183*Ti_Si)+(4.9886*Al_Si)+(2.2204*Fe_Si)+(1.1801*Feo_Si)+(-0.3008*Mn_Si)+(1.3297*Mg_Si)+(-2.1834*Ca_Si)+(-1.9319*Na_Si)+(0.6976*K_Si)+(0.8998*P_Si)+13.2625
    DF2_B5=(5.0509*Ti_Si)+(-0.4972*Al_Si)+(1.0046*Fe_Si)+(-3.3848*Feo_Si)+(0.5528*Mn_Si)+(0.2925*Mg_Si)+(0.4007*Ca_Si)+(-2.8637*Na_Si)+(-0.2189*K_Si)+(-1.0558*P_Si)+2.8877
    #### IAB-CRB-OIB-MORB ###
    x1=c(3.431,1.16,1.16,1.16,5.912,1.16,-0.266,-0.266,-4.19,-0.266,-8)
    y1=c(-8,-0.333,-0.333,-0.333,8,-0.333,0.02,0.02,8,0.02,-2.49)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(-5,2,'OIB (71%)')
    text(1,5,'CRB (84%)')
    text(6,0,'IAB (94%)')
    text(-4,-4,'MORB(97%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IAB-CRB-OIB ###
    x1=c(-0.733,-3.788,-0.733,8,-0.733,-1.343)
    y1=c(-1.405,8,-1.405,5.428,-1.405,-8)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-OIB)))),ylab=c(expression(bold(DF2(IAB-CRB-OIB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,0,'IAB (94%)')
    text(0,5,'CRB (83%)')
    text(5,-3,'OIB (83%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    ####IAB-CRB-MORB ###
    x1=c(8,0.361,-2.673,0.361,-6.779,0.361)
    y1=c(-1.331,-0.619,8,-0.619,-8,-0.619)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-CRB-MORB)))),ylab=c(expression(bold(DF2(IAB-CRB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,4,'MORB (99%)')
    text(2,5,'CRB (97%)')
    text(3,-3,'IAB (94%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IAB-OIB-MORB ###
    x1=c(-0.830,-1.824,-0.830,8,-0.830,-4.865)
    y1=c(1.224,8,1.224,-3.583,1.224,-8)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IAB-OIB-MORB)))),ylab=c(expression(bold(DF2(IAB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,4,'IAB (99%)')
    text(0,-5,'MORB (100%)')
    text(2,5,'OIB (84%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CRB-OIB-MORB ###
    x1=c(0.029,8,0.029,-6.177,0.029,-0.819)
    y1=c(-0.222,4.322,-0.222,8,-0.222,-8)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(CRB-OIB-MORB)))),ylab=c(expression(bold(DF2(CRB-OIB-MORB)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,-2,'CRB (83%)')
    text(-5,0,'MORB (99%)')
    text(0,5,'OIB (71%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }
  B=readline('Did you want to generate a report with the DF and log ratios values? Yes=(Y),No=(N)')
  if (B=='Y'){
    X=D[,2]
    x=D[,1] ### Colocar la posici?n de la muestra
    fa=data.frame(D,X,Ti_Si,Al_Si,Fe_Si,Feo_Si,Mn_Si,Mg_Si,Ca_Si,Na_Si,K_Si,P_Si,X,DF2_B1,DF1_B2,DF2_B2,DF1_B3,DF2_B3,DF1_B4,DF2_B4,DF1_B5,DF2_B5)
    write.csv(fa,file='Report_Verma_etal_2006.csv',row.names=T)
  }else if (B=='N'){
    b=print('Thanks, I invite you to generate more diagrams at www.platicandodelatierra.com ')
  }
}