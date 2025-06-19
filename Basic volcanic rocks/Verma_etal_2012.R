rm(list=ls())  #borra todas las variables que esten asignados
graphics.off() #cierra todas las graficas 
##### Diagramas de discriminaci?n tect?nica log ###
readline('Welcome ! lets generate some diagrams (please push enter to continue)')
Di=readline('Please type Verma_etal_2012')
if(Di=='Verma_etal_2012'){ ### granitic acid rocks
  A=readline('Did you want the diagrams all together? Yes=(Y),No=(N)')
  if (substr(A,1,1)=='Y'){
    x11()
    par(mfrow=c(2,3))
    ##### Datos y funciones ##
    D=read.csv('Verma_etal_2012.csv')
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
    ## Funcion de IA+CA-CR-Col
    DF1_B1=(0.36077*Ti_Si)+(0.95693*Al_Si)+(-2.09239*Fe_Si)+(0.93391*Feo_Si)+(0.42703*Mn_Si)+(0.18732*Mg_Si)+(0.45615*Ca_Si)+(0.56098*Na_Si)+(-1.65167*K_Si)+(-0.15580*P_Si)-1.58259
    DF2_B1=(0.472353*Ti_Si)+(-0.954629*Al_Si)+(0.109516*Fe_Si)+(0.699238*Feo_Si)+(0.739533*Mn_Si)+(-0.027717*Mg_Si)+(-0.244687*Ca_Si)+(0.231677*Na_Si)+(0.173552*K_Si)+(-.353797*P_Si)+6.691035
    ## Funcion de IA-CA-CR 
    DF1_B2=(-0.4786*Ti_Si)+(-0.0871*Al_Si)+(2.7433*Fe_Si)+(-1.0663*Feo_Si)+(-0.1389*Mn_Si)+(-0.1907*Mg_Si)+(-0.8516*Ca_Si)+(-0.7139*Na_Si)+(1.7166*K_Si)+(0.3386*P_Si)+6.2573
    DF2_B2=(-0.3204*Ti_Si)+(-1.7585*Al_Si)+(-3.2046*Fe_Si)+(1.1210*Feo_Si)+(0.2170*Mn_Si)+(-0.0745*Mg_Si)+(1.2505*Ca_Si)+(1.3142*Na_Si)+(1.6616*K_Si)+(0.0186*P_Si)+0.9984
    ## Funcion de IA-CA-Col    
    DF1_B3=(-0.3620*Ti_Si)+(-0.0342*Al_Si)+(0.5198*Fe_Si)+(-0.4980*Feo_Si)+(-0.7223*Mn_Si)+(-0.1229*Mg_Si)+(-0.1388*Ca_Si)+(-0.8174*Na_Si)+(1.5074*K_Si)+(0.2684*P_Si)-3.0829
    DF2_B3=(-0.142*Ti_Si)+(1.984*Al_Si)+(1.747*Fe_Si)+(-0.735*Feo_Si)+(-1.226*Mn_Si)+(0.062*Mg_Si)+(-1.152*Ca_Si)+(-3.189*Na_Si)+(-2.339*K_Si)+(0.495*P_Si)-18.190
    ## Funcion de IA-CR-Col      
    DF1_B4=(0.0226*Ti_Si)+(1.2877*Al_Si)+(-2.6406*Fe_Si)+(2.9494*Feo_Si)+(0.1970*Mn_Si)+(0.0673*Mg_Si)+(0.0620*Ca_Si)+(0.6219*Na_Si)+(-2.0579*K_Si)+(-0.0751*P_Si)-2.1790
    DF2_B4=(0.2786*Ti_Si)+(-1.0544*Al_Si)+(0.8267*Fe_Si)+(0.3032*Feo_Si)+(0.4084*Mn_Si)+(-0.0905*Mg_Si)+(-0.3260*Ca_Si)+(0.1518*Na_Si)+(0.6698*K_Si)+(-0.2261*P_Si)+6.5170
    ## Funcion de CA-CR-Col   
    DF1_B5=(0.0645*Ti_Si)+(-1.7943*Al_Si)+(0.5264*Fe_Si)+(0.6385*Feo_Si)+(0.3407*Mn_Si)+(-0.0720*Mg_Si)+(-0.3265*Ca_Si)+(0.1063*Na_Si)+(1.8098*K_Si)+(-0.0338*P_Si)+8.2616
    DF2_B5=(0.8760*Ti_Si)+(0.8018*Al_Si)+(0.2472*Fe_Si)+(-0.8796*Feo_Si)+(0.7540*Mn_Si)+(-0.0006*Mg_Si)+(-0.0624*Ca_Si)+(-0.2052*Na_Si)+(-3.3091*K_Si)+(-0.3526*P_Si)-3.8959
    #### IA+CA-CR-Col ###
    x1=c(3.0914,-0.52237,6.5177,-0.52237,-8,-0.52237)
    y1=c(8,0.105108,-8,0.105108,-1.6511,0.105108)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB+CA-CR-Col)))),ylab=c(expression(bold(DF2(IAB+CA-CR-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(5,-2,'IA(96%)+CA(56)')
    text(-4,2,'CR(72%)')
    text(0,-5,'Col(80%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IA-CA-CR ###
    x1=c(-8,0.41929,1.0939,0.41929,4.1608,0.41929)
    y1=c(4.7147,-0.66705,-8,-0.66705,8,-0.66705)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR)))),ylab=c(expression(bold(DF2(IA-CA-CR)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-3,-3,'IA(76%)')
    text(-2,5,'CA(92%)')
    text(5,0,'CR(76%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IA-CA-Col ###
    x1=c(-6.8768,0.13893,0.39469,0.13893,4.1472,0.13893)
    y1=c(-8,1.18829,8,1.18829,-8,1.18829)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,2,'IA(76%)')
    text(0,-4,'CA(64%)')
    text(5,2,'Col(72%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IA-CR-Col ###
    x1=c(4.7956,0.20518,2.1584,0.20518,-8,0.20518)
    y1=c(8,-0.01689,-8,-0.01689,1.61186,-0.01689)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR-Col)))),ylab=c(expression(bold(DF2(IA-CR-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,2,'IA(88%)')
    text(-4,5,'CR(84%)')
    text(-4,-4,'Col(84%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CA-CR-Col ###
    x1=c(4.6620,0.22442,-8,0.22442,3.3907,0.22442)
    y1=c(8,0.015552,0.53675,0.015552,-8,0.015552)
    plot(DF1_B5,DF2_B5,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR-Col)))),ylab=c(expression(bold(DF2(CA-CR-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-3,5,'CA(60%)')
    text(5,0,'CR(76%)')
    text(-4,-3,'Col(76%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }else if (A=='N'){
    ##### Datos y funciones ##
    D=read.csv('Verma_etal_2012.csv')
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
    ## Funcion de IA+CA-CR-Col
    DF1_B1=(0.36077*Ti_Si)+(0.95693*Al_Si)+(-2.09239*Fe_Si)+(0.93391*Feo_Si)+(0.42703*Mn_Si)+(0.18732*Mg_Si)+(0.45615*Ca_Si)+(0.56098*Na_Si)+(-1.65167*K_Si)+(-0.15580*P_Si)-1.58259
    DF2_B1=(0.472353*Ti_Si)+(-0.954629*Al_Si)+(0.109516*Fe_Si)+(0.699238*Feo_Si)+(0.739533*Mn_Si)+(-0.027717*Mg_Si)+(-0.244687*Ca_Si)+(0.231677*Na_Si)+(0.173552*K_Si)+(-.353797*P_Si)+6.691035
    ## Funcion de IA-CA-CR 
    DF1_B2=(-0.4786*Ti_Si)+(-0.0871*Al_Si)+(2.7433*Fe_Si)+(-1.0663*Feo_Si)+(-0.1389*Mn_Si)+(-0.1907*Mg_Si)+(-0.8516*Ca_Si)+(-0.7139*Na_Si)+(1.7166*K_Si)+(0.3386*P_Si)+6.2573
    DF2_B2=(-0.3204*Ti_Si)+(-1.7585*Al_Si)+(-3.2046*Fe_Si)+(1.1210*Feo_Si)+(0.2170*Mn_Si)+(-0.0745*Mg_Si)+(1.2505*Ca_Si)+(1.3142*Na_Si)+(1.6616*K_Si)+(0.0186*P_Si)+0.9984
    ## Funcion de IA-CA-Col    
    DF1_B3=(-0.3620*Ti_Si)+(-0.0342*Al_Si)+(0.5198*Fe_Si)+(-0.4980*Feo_Si)+(-0.7223*Mn_Si)+(-0.1229*Mg_Si)+(-0.1388*Ca_Si)+(-0.8174*Na_Si)+(1.5074*K_Si)+(0.2684*P_Si)-3.0829
    DF2_B3=(-0.142*Ti_Si)+(1.984*Al_Si)+(1.747*Fe_Si)+(-0.735*Feo_Si)+(-1.226*Mn_Si)+(0.062*Mg_Si)+(-1.152*Ca_Si)+(-3.189*Na_Si)+(-2.339*K_Si)+(0.495*P_Si)-18.190
    ## Funcion de IA-CR-Col      
    DF1_B4=(0.0226*Ti_Si)+(1.2877*Al_Si)+(-2.6406*Fe_Si)+(2.9494*Feo_Si)+(0.1970*Mn_Si)+(0.0673*Mg_Si)+(0.0620*Ca_Si)+(0.6219*Na_Si)+(-2.0579*K_Si)+(-0.0751*P_Si)-2.1790
    DF2_B4=(0.2786*Ti_Si)+(-1.0544*Al_Si)+(0.8267*Fe_Si)+(0.3032*Feo_Si)+(0.4084*Mn_Si)+(-0.0905*Mg_Si)+(-0.3260*Ca_Si)+(0.1518*Na_Si)+(0.6698*K_Si)+(-0.2261*P_Si)+6.5170
    ## Funcion de CA-CR-Col   
    DF1_B5=(0.0645*Ti_Si)+(-1.7943*Al_Si)+(0.5264*Fe_Si)+(0.6385*Feo_Si)+(0.3407*Mn_Si)+(-0.0720*Mg_Si)+(-0.3265*Ca_Si)+(0.1063*Na_Si)+(1.8098*K_Si)+(-0.0338*P_Si)+8.2616
    DF2_B5=(0.8760*Ti_Si)+(0.8018*Al_Si)+(0.2472*Fe_Si)+(-0.8796*Feo_Si)+(0.7540*Mn_Si)+(-0.0006*Mg_Si)+(-0.0624*Ca_Si)+(-0.2052*Na_Si)+(-3.3091*K_Si)+(-0.3526*P_Si)-3.8959
    #### IA+CA-CR-Col ###
    x1=c(3.0914,-0.52237,6.5177,-0.52237,-8,-0.52237)
    y1=c(8,0.105108,-8,0.105108,-1.6511,0.105108)
    plot(DF1_B1,DF2_B1,pch=2,col='red',xlab=c(expression(bold(DF1(IAB+CA-CR-Col)))),ylab=c(expression(bold(DF2(IAB+CA-CR-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
    text(5,-2,'IA(96%)+CA(56)')
    text(-4,2,'CR(72%)')
    text(0,-5,'Col(80%)')
    rect(-8,-8,8,8);
    axis(1, at=seq(-8,8,by=2),labels=T);
    axis(2, at=seq(-8,8,by=2),labels=T);
    lines(x1,y1,lwd=2);
    #### IA-CA-CR ###
    x1=c(-8,0.41929,1.0939,0.41929,4.1608,0.41929)
    y1=c(4.7147,-0.66705,-8,-0.66705,8,-0.66705)
    plot(DF1_B2,DF2_B2,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR)))),ylab=c(expression(bold(DF2(IA-CA-CR)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-3,-3,'IA(76%)')
    text(-2,5,'CA(92%)')
    text(5,0,'CR(76%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IA-CA-Col ###
    x1=c(-6.8768,0.13893,0.39469,0.13893,4.1472,0.13893)
    y1=c(-8,1.18829,8,1.18829,-8,1.18829)
    plot(DF1_B3,DF2_B3,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-5,2,'IA(76%)')
    text(0,-4,'CA(64%)')
    text(5,2,'Col(72%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### IA-CR-Col ###
    x1=c(4.7956,0.20518,2.1584,0.20518,-8,0.20518)
    y1=c(8,-0.01689,-8,-0.01689,1.61186,-0.01689)
    plot(DF1_B4,DF2_B4,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR-Col)))),ylab=c(expression(bold(DF2(IA-CR-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(5,2,'IA(88%)')
    text(-4,5,'CR(84%)')
    text(-4,-4,'Col(84%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
    #### CA-CR-Col ###
    x1=c(4.6620,0.22442,-8,0.22442,3.3907,0.22442)
    y1=c(8,0.015552,0.53675,0.015552,-8,0.015552)
    plot(DF1_B5,DF2_B5,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR-Col)))),ylab=c(expression(bold(DF2(CA-CR-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F)
    text(-3,5,'CA(60%)')
    text(5,0,'CR(76%)')
    text(-4,-3,'Col(76%)')
    rect(-8,-8,8,8)
    axis(1, at=seq(-8,8,by=2),labels=T)
    axis(2, at=seq(-8,8,by=2),labels=T)
    lines(x1,y1,lwd=2)
  }
  B=readline('Did you want to generate a report with the DF and log ratios values? Yes=(Y),No=(N)')
  if (B=='Y'){
    X=D[,2]
    x=D[,1] ### Colocar la posici?n de la muestra
    fa=data.frame(D,X,Ti_Si,Al_Si,Fe_Si,Feo_Si,Mn_Si,Mg_Si,Ca_Si,Na_Si,K_Si,P_Si,X,DF1_B1,DF2_B1,DF1_B2,DF2_B2,DF1_B3,DF2_B3,DF1_B4,DF2_B4,DF1_B5,DF2_B5)
    write.csv(fa,file='Report_Verma_2012.csv',row.names=T)
  }else if (B=='N'){
    b=print('Thanks, I invite you to generate more diagrams at www.platicandodelatierra.com ')
  }
}