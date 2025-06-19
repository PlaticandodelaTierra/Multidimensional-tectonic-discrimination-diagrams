rm(list=ls())  #borra todas las variables que esten asignados
graphics.off() #cierra todas las graficas 
##### Diagramas de discriminaci?n tect?nica log ###
readline('Welcome ! lets generate some diagrams (please push enter to continue)')
Di=readline('Please type Verma_etal_2013_int')
A=readline('Did you want the diagrams all together? Yes=(Y),No=(N)')
if (substr(A,1,1)=='Y'){
  #########1############
  x11()
  par(mfrow=c(2,3))
  ##### Datos y funciones ##
  D=read.csv('Verma_etal_2013_int.csv')
  ### ppm to percentage 
  La=(D[,14]*0.0001)/1 
  Yb=(D[,15]*0.0001)/1
  Sm=(D[,16]*0.0001)/1
  Th=(D[,17]*0.0001)/1
  Nb=(D[,18]*0.0001)/1
  Ni=(D[,19]*0.0001)/1
  V=(D[,20]*0.0001)/1
  Y=(D[,21]*0.0001)/1
  Zr=(D[,22]*0.0001)/1
  #### Relaciones logaritmicas 1 ##
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
  #### Funcion IA+CA-CR+OI-Col ##
  DF1_INT1=(-2.45605*Ti_Si)+(1.11985*Al_Si)+(-2.22475*Fe_Si)+(2.48861*Feo_Si)+(-0.212024*Mn_Si)+(-0.06661*Mg_Si)+(1.29066*Ca_Si)+(-0.28377*Na_Si)+(-0.40211*K_Si)+(0.030635*P_Si)-11.43097347
  DF2_INT1=(-0.57759*Ti_Si)+(-0.01121*Al_Si)+(0.69125*Fe_Si)+(-1.99798*Feo_Si)+(-1.72014*Mn_Si)+(0.305275*Mg_Si)+(0.816018*Ca_Si)+(-1.791727*Na_Si)+(0.871298*K_Si)+(0.335479*P_Si)-12.20158596
  #### Funcion IA-CA-CR+OI ##
  DF1_INT2=(-2.51880*Ti_Si)+(0.54210*Al_Si)+(-3.790190*Fe_Si)+(3.846277*Feo_Si)+(-0.362718*Mn_Si)+(-0.176632*Mg_Si)+(1.426496*Ca_Si)+(0.111801*Na_Si)+(-0.219223*K_Si)+(-0.07248*P_Si)-14.3151255
  DF2_INT2=(-1.04907*Ti_Si)+(3.440438*Al_Si)+(-3.43323*Fe_Si)+(4.807165*Feo_Si)+(-3.499257*Mn_Si)+(0.373928*Mg_Si)+(-2.147775*Ca_Si)+(3.00229*Na_Si)+(-0.773719*K_Si)+(1.061808*P_Si)-13.4885545
  #### Funcion IA-CA-Col ##
  DF1_INT3=(-0.88680*Ti_Si)+(-0.781835*Al_Si)+(-2.43157*Fe_Si)+(4.10644*Feo_Si)+(2.050295*Mn_Si)+(-0.386860*Mg_Si)+(-0.74023*Ca_Si)+(1.35997*Na_Si)+(-0.816315*K_Si)+(-0.468418*P_Si)+4.31214432
  DF2_INT3=(1.76033*Ti_Si)+(-4.32894*Al_Si)+(2.60111*Fe_Si)+(-4.96088*Feo_Si)+(2.89683*Mn_Si)+(-0.362075*Mg_Si)+(2.23018*Ca_Si)+(-2.96677*Na_Si)+(0.790236*K_Si)+(-1.326438*P_Si)+7.586117348
  #### Funcion IA-CR+OI-Col ##
  DF1_INT4=(-2.43565*Ti_Si)+(1.53913*Al_Si)+(-1.51665*Fe_Si)+(1.45582*Feo_Si)+(0.4961937*Mn_Si)+(-0.050128*Mg_Si)+(1.258138*Ca_Si)+(-0.8274299*Na_Si)+(-0.4884699*K_Si)+(0.1123605*P_Si)-7.894955173
  DF2_INT4=(-0.736658*Ti_Si)+(-0.0788099*Al_Si)+(0.065533*Fe_Si)+(-1.130176*Feo_Si)+(-2.130889*Mn_Si)+(0.245709*Mg_Si)+(0.681694*Ca_Si)+(-1.3284307*Na_Si)+(0.7709408*K_Si)+(0.295664*P_Si)-15.24062267
  #### Funcion CA-CR+OI-Col ##
  DF1_INT5=(-2.32173*Ti_Si)+(1.97128*Al_Si)+(-0.537435*Fe_Si)+(0.431388*Feo_Si)+(-1.139286*Mn_Si)+(0.527984*Mg_Si)+(0.9884038*Ca_Si)+(-0.894467*Na_Si)+(0.16138688*K_Si)+(0.0778358*P_Si)-12.34961873
  DF2_INT5=(-0.40691*Ti_Si)+(2.60576*Al_Si)+(0.1610669*Fe_Si)+(1.345967*Feo_Si)+(0.4457959*Mn_Si)+(-0.260127*Mg_Si)+(-0.464534*Ca_Si)+(0.9211739*Na_Si)+(-1.2769499*K_Si)+(-0.142884*P_Si)+3.501318155
  #### Funcion IA+CA-CR+OI-Col ##
  x1=c(0.42744,-0.67554,8,-0.67554,-8,-0.67554)
  y1=c(-8,0.27663,5.53331,0.27663,4.73569,0.27663)
  plot(DF1_INT1,DF2_INT1,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(5,-2,'IA(90%)+CA(79.3%)')
  text(-4,-4,'CR(71.6%)+OI(96.4%)')
  text(-1,5,'Col(86.8%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-CR+OI ##
  x1=c(8,-0.63205,-1.50230,-0.63205,-2.73408,-0.63205)
  y1=c(0.76690,0.08764,-8,0.08764,8,0.08764)
  plot(DF1_INT2,DF2_INT2,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(5,-2,'IA(71.3%)')
  text(-4,2,'CR(76.6%)+OI(93.9%)')
  text(4,4,'CA(72.5%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-Col ##
  x1=c(8,-0.71170,-1.18110,-0.71170,-3.55140,-0.71170)
  y1=c(-3.06676,0.24138,8,0.24138,-8,0.24138)
  plot(DF1_INT3,DF2_INT3,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,0,'Col(85.5%)')
  text(4,-4,'CA(69.1%)')
  text(5,4,'IA(69.1%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CR+OI-Col ##
  x1=c(0.6676,-0.44102,8,-0.44102,-8,-0.44102)
  y1=c(-8,0.17933,6.27226,0.17933,4.24657,0.17933)
  plot(DF1_INT4,DF2_INT4,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,-3,'CR(75%)+OI(94.9%)')
  text(5,-3,'IA(89%)')
  text(-1,5,'Col(86.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion CA-CR+OI-Col ##
  x1=c(-3.42497,-0.033967,8,-0.033967,-4.17272,-0.033967)
  y1=c(8,-0.10997,-0.16286,-0.10997,-8,-0.10997)
  plot(DF1_INT5,DF2_INT5,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,-2,'CR(71.6%)+OI(96%)')
  text(5,-4,'Col(85.5%)')
  text(4,4,'CA(80%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  ##########2############
  x11()
  par(mfrow=c(2,3))
  #### Relaciones logaritmicas 2 ##
  Mg_Ti=log(D[,9]/D[,4])
  P_Ti=log(D[,13]/D[,4])
  Nb_Ti=log(Nb/D[,4])
  Ni_Ti=log(Ni/D[,4])
  V_Ti=log(V/D[,4])
  Y_Ti=log(Y/D[,4])
  Zr_Ti=log(Zr/D[,4])
  #### Funcion IA+CA-CR+OI-Col##
  DF1_INT6=(1.02293*Mg_Ti)+(0.63053*P_Ti)+(-0.93889*Nb_Ti)+(-0.41538*Ni_Ti)+(1.676898*V_Ti)+(0.453813*Y_Ti)+(0.5831823*Zr_Ti)+1.900726416
  DF2_INT6=(0.248529*Mg_Ti)+(-0.477177*P_Ti)+(-0.33628*Nb_Ti)+(-0.131072*Ni_Ti)+(-1.712035*V_Ti)+(0.213840*Y_Ti)+(-2.008435*Zr_Ti)-18.63750138
  #### Funcion IA-CA-CR+OI ##
  DF1_INT7=(0.8750597*Mg_Ti)+(0.4279822*P_Ti)+(-0.6864967*Nb_Ti)+(-0.372419*Ni_Ti)+(1.924254*V_Ti)+(0.835240*Y_Ti)+(0.8428416*Zr_Ti)+8.228368089
  DF2_INT7=(-1.171625*Mg_Ti)+(-2.650912*P_Ti)+(0.176065*Nb_Ti)+(0.1183849*Ni_Ti)+(-0.18532798*V_Ti)+(1.9213464*Y_Ti)+(0.3868149*Zr_Ti)+12.45160186
  #### Funcion IA-CA-Col ##
  DF1_INT8=(-0.801371*Mg_Ti)+(0.125028*P_Ti)+(0.908386*Nb_Ti)+(0.320442*Ni_Ti)+(-0.3683636*V_Ti)+(-0.6405805*Y_Ti)+(0.72337227*Zr_Ti)+8.1087217398
  DF2_INT8=(1.317201*Mg_Ti)+(2.199955*P_Ti)+(-0.1235449*Nb_Ti)+(-0.1339018*Ni_Ti)+(-0.8720114*V_Ti)+(-1.7825807*Y_Ti)+(-1.36498299*Zr_Ti)-20.63036447
  #### Funcion IA-CR+OI-Col ##
  DF1_INT9=(-0.85601*Mg_Ti)+(-0.300589*P_Ti)+(0.861909*Nb_Ti)+(0.384727*Ni_Ti)+(-1.5827037*V_Ti)+(-0.757282*Y_Ti)+(-0.692422*Zr_Ti)-4.468550646
  DF2_INT9=(0.21504*Mg_Ti)+(-0.503675*P_Ti)+(-0.32252*Nb_Ti)+(-0.122383*Ni_Ti)+(-1.7097486*V_Ti)+(0.426039*Y_Ti)+(-1.980676*Zr_Ti)-17.04082095
  #### Funcion CA-CR+OI-Col ##
  DF1_INT10=(-1.25554*Mg_Ti)+(-1.082014*P_Ti)+(1.437934*Nb_Ti)+(0.5454469*Ni_Ti)+(-1.6196297*V_Ti)+(0.3368725*Y_Ti)+(-0.71359906*Zr_Ti)+5.752160917
  DF2_INT10=(-0.02400*Mg_Ti)+(-0.054413*P_Ti)+(-0.8608025*Nb_Ti)+(-0.174160*Ni_Ti)+(-1.6407186*V_Ti)+(0.068523*Y_Ti)+(-1.772088*Zr_Ti)-21.02758313
  #### Funcion IA+CA-CR+OI-Col ##
  x1=c(0.92190,-0.82858,6.39297,-0.82858,-8,-0.82858)
  y1=c(8,0.29965,-8,0.29965,-4.20284,0.29965)
  plot(DF1_INT6,DF2_INT6,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,4,'CR(72.9%)+OI(100%)')
  text(-4,-6,'Col(90.2%)')
  text(5,-2,'IA(86.3%)+CA(88.5%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-CR+OI ##
  x1=c(8,-0.95018,-1.24490,-0.95018,-3.41007,-0.95018)
  y1=c(-3.76290,0.45941,8,0.45941,-8,0.45941)
  plot(DF1_INT7,DF2_INT7,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,-4,'CR(79.2%)+OI(100%)')
  text(2,-5,'CA(81.8%)')
  text(5,5,'IA(70.4%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-Col ##
  x1=c(-8,0.60491,0.95093,0.60491,4.00195,0.60491)
  y1=c(3.71126,-0.23211,-8,-0.23211,8,-0.23211)
  plot(DF1_INT8,DF2_INT8,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,-2,'IA(62.8%)')
  text(-3,5,'CA(76.2%)')
  text(5,3,'CoL(92.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CR+OI-Col ##
  x1=c(-0.87616,0.62149,-6.61289,0.62149,8,0.62149)
  y1=c(8,0.34939,-8,0.34939,-4.51524,0.34939)
  plot(DF1_INT9,DF2_INT9,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,0,'IA(85.5%)')
  text(0,-6,'Col(90.2%)')
  text(5,3,'CR(74.2%)+OI(98.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion CA-CR+OI-Col ##
  x1=c(-1.16430,-0.028516,-7.33632,-0.028516,8,-0.028516)
  y1=c(8,0.35743,-8,0.35743,-3.84452,0.35743)
  plot(DF1_INT10,DF2_INT10,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-6,0,'CA(94.7%)')
  text(0,-5,'Col(90.8%)')
  text(5,3,'CR(72.9%)+OI(98.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #######3##############
  x11()
  par(mfrow=c(2,3))
  #### Relaciones logaritmicas 3 ##
  La_Yb=log(D[,14]/D[,15])
  Ce_Yb=log(D[,23]/D[,15])
  Sm_Yb=log(D[,16]/D[,15])
  Nb_Yb=log(D[,18]/D[,15])
  Th_Yb=log(D[,17]/D[,15])
  Y_Yb=log(D[,21]/D[,15])
  Zr_Yb=log(D[,22]/D[,15])
  #### Funcion IA+CA-CR+OI-Col ##
  DF1_INT11=(-0.1672589*La_Yb)+(-1.2542899*Ce_Yb)+(1.295171*Sm_Yb)+(1.3318361*Nb_Yb)+(0.2698636*Th_Yb)+(1.9286976*Y_Yb)+(0.18097357*Zr_Yb)-3.815745639
  DF2_INT11=(-0.2426713*La_Yb)+(1.7265475*Ce_Yb)+(0.490224*Sm_Yb)+(-1.2755648*Nb_Yb)+(0.9602491*Th_Yb)+(0.8511852*Y_Yb)+(-0.4894082*Zr_Yb)-3.305510646
  #### Funcion IA-CA-CR+OI ##
  DF1_INT12=(0.0178001*La_Yb)+(-1.2689712*Ce_Yb)+(1.7407108*Sm_Yb)+(1.324421438*Nb_Yb)+(0.0288819*Th_Yb)+(1.580888497*Y_Yb)+(0.17161461*Zr_Yb)-3.3845534709
  DF2_INT12=(-2.099551*La_Yb)+(-2.044178*Ce_Yb)+(-0.41179008*Sm_Yb)+(1.022466699*Nb_Yb)+(1.24448424*Th_Yb)+(1.87700276*Y_Yb)+(1.07017399797*Zr_Yb)-0.2920468400
  #### Funcion IA-CA-Col ##
  DF1_INT13=(0.092724*La_Yb)+(0.752143*Ce_Yb)+(0.9296053*Sm_Yb)+(0.12351021*Nb_Yb)+(0.3479451*Th_Yb)+(1.472513*Y_Yb)+(-0.0339674*Zr_Yb)-5.801482381
  DF2_INT13=(-2.038286*La_Yb)+(-0.073322*Ce_Yb)+(-1.360432*Sm_Yb)+(-0.0782899*Nb_Yb)+(1.8248761*Th_Yb)+(2.7738488*Y_Yb)+(0.44440139*Zr_Yb)-3.684349292
  #### Funcion IA-CR+OI-Col ##
  DF1_INT14=(0.720851*La_Yb)+(-1.352147*Ce_Yb)+(1.378563*Sm_Yb)+(1.1641465*Nb_Yb)+(-0.0423769*Th_Yb)+(1.5584709*Y_Yb)+(-0.1644980*Zr_Yb)-2.9336489118
  DF2_INT14=(0.2378909*La_Yb)+(-2.03548886*Ce_Yb)+(-0.2501036699*Sm_Yb)+(1.34733326*Nb_Yb)+(-0.760673982*Th_Yb)+(-0.786605747*Y_Yb)+(0.37736968328*Zr_Yb)+4.154732286
  #### Funcion CA-CR+OI-Col ##
  DF1_INT15=(-0.977026*La_Yb)+(-1.3886489*Ce_Yb)+(1.36560*Sm_Yb)+(1.8999127*Nb_Yb)+(0.5690460*Th_Yb)+(1.65772638*Y_Yb)+(-0.30523813*Zr_Yb)-0.87680549008
  DF2_INT15=(-0.086967*La_Yb)+(1.1636159*Ce_Yb)+(0.3635930*Sm_Yb)+(-0.90127239*Nb_Yb)+(1.1257989*Th_Yb)+(1.19149068*Y_Yb)+(-0.39964298*Zr_Yb)-3.915383182
  #### Funcion IA+CA-CR+OI-Col ##
  x1=c(-0.69292,0.64148,-6.91145,0.64148,8,0.64148)
  y1=c(-8,0.34301,8,0.34301,3.04640,0.34301)
  plot(DF1_INT11,DF2_INT11,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,1,'IA(91.4%)+CA(90.4%)')
  text(5,-3,'CR(74.3%)+OI(100%)')
  text(4,6,'Col(81%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-CR+OI ##
  x1=c(-8,0.58959,0.87619,0.58959,3.67939,0.58959)
  y1=c(-5.45793,0.68699,8,0.68699,-8,0.68699)
  plot(DF1_INT12,DF2_INT12,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,3,'IA(75.7%)')
  text(-3,-5,'CA(65.8%)')
  text(6,-4,'CR(80.5%)+OI(100%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-Col ##
  x1=c(-8,0.90473,0.64537,0.90473,4.86730,0.90473)
  y1=c(-7.28196,0.82230,8,0.82230,-8,0.82230)
  plot(DF1_INT13,DF2_INT13,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,2,'IA(72.7%)')
  text(2,-5,'CA(64.5%)')
  text(6,-5,'Col(84%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CR+OI-Col ##
  x1=c(-0.87235,0.37157,-6.10890,0.37157,8,0.37157)
  y1=c(8,-0.26385,-8,-0.26385,-2.82217,-0.26385)
  plot(DF1_INT14,DF2_INT14,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-6,0,'IA(90.3%)')
  text(5,3,'CR(74.7%)+OI(100%)')
  text(0,-5,'Col(84.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion CA-CR+OI-Col ##
  x1=c(-0.10284,-0.15459,-8,-0.15459,8,-0.15459)
  y1=c(-8,0.29462,5.41425,0.29462,4.74335,0.29462)
  plot(DF1_INT15,DF2_INT15,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,-3,'CA(95.7%)')
  text(5,-3,'CR(74.3%)+OI(94.1%)')
  text(2,6,'Col(81%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
}else if (A=='N'){
  ##### Datos y funciones ##
  D=read.csv('Verma_etal_2013_int.csv')
  ### ppm to percentage 
  La=(D[,14]*0.0001)/1 
  Yb=(D[,15]*0.0001)/1
  Sm=(D[,16]*0.0001)/1
  Th=(D[,17]*0.0001)/1
  Nb=(D[,18]*0.0001)/1
  Ni=(D[,19]*0.0001)/1
  V=(D[,20]*0.0001)/1
  Y=(D[,21]*0.0001)/1
  Zr=(D[,22]*0.0001)/1
  #### Relaciones logaritmicas 1 ##
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
  #### Funcion IA+CA-CR+OI-Col ##
  DF1_INT1=(-2.45605*Ti_Si)+(1.11985*Al_Si)+(-2.22475*Fe_Si)+(2.48861*Feo_Si)+(-0.212024*Mn_Si)+(-0.06661*Mg_Si)+(1.29066*Ca_Si)+(-0.28377*Na_Si)+(-0.40211*K_Si)+(0.030635*P_Si)-11.43097347
  DF2_INT1=(-0.57759*Ti_Si)+(-0.01121*Al_Si)+(0.69125*Fe_Si)+(-1.99798*Feo_Si)+(-1.72014*Mn_Si)+(0.305275*Mg_Si)+(0.816018*Ca_Si)+(-1.791727*Na_Si)+(0.871298*K_Si)+(0.335479*P_Si)-12.20158596
  #### Funcion IA-CA-CR+OI ##
  DF1_INT2=(-2.51880*Ti_Si)+(0.54210*Al_Si)+(-3.790190*Fe_Si)+(3.846277*Feo_Si)+(-0.362718*Mn_Si)+(-0.176632*Mg_Si)+(1.426496*Ca_Si)+(0.111801*Na_Si)+(-0.219223*K_Si)+(-0.07248*P_Si)-14.3151255
  DF2_INT2=(-1.04907*Ti_Si)+(3.440438*Al_Si)+(-3.43323*Fe_Si)+(4.807165*Feo_Si)+(-3.499257*Mn_Si)+(0.373928*Mg_Si)+(-2.147775*Ca_Si)+(3.00229*Na_Si)+(-0.773719*K_Si)+(1.061808*P_Si)-13.4885545
  #### Funcion IA-CA-Col ##
  DF1_INT3=(-0.88680*Ti_Si)+(-0.781835*Al_Si)+(-2.43157*Fe_Si)+(4.10644*Feo_Si)+(2.050295*Mn_Si)+(-0.386860*Mg_Si)+(-0.74023*Ca_Si)+(1.35997*Na_Si)+(-0.816315*K_Si)+(-0.468418*P_Si)+4.31214432
  DF2_INT3=(1.76033*Ti_Si)+(-4.32894*Al_Si)+(2.60111*Fe_Si)+(-4.96088*Feo_Si)+(2.89683*Mn_Si)+(-0.362075*Mg_Si)+(2.23018*Ca_Si)+(-2.96677*Na_Si)+(0.790236*K_Si)+(-1.326438*P_Si)+7.586117348
  #### Funcion IA-CR+OI-Col ##
  DF1_INT4=(-2.43565*Ti_Si)+(1.53913*Al_Si)+(-1.51665*Fe_Si)+(1.45582*Feo_Si)+(0.4961937*Mn_Si)+(-0.050128*Mg_Si)+(1.258138*Ca_Si)+(-0.8274299*Na_Si)+(-0.4884699*K_Si)+(0.1123605*P_Si)-7.894955173
  DF2_INT4=(-0.736658*Ti_Si)+(-0.0788099*Al_Si)+(0.065533*Fe_Si)+(-1.130176*Feo_Si)+(-2.130889*Mn_Si)+(0.245709*Mg_Si)+(0.681694*Ca_Si)+(-1.3284307*Na_Si)+(0.7709408*K_Si)+(0.295664*P_Si)-15.24062267
  #### Funcion CA-CR+OI-Col ##
  DF1_INT5=(-2.32173*Ti_Si)+(1.97128*Al_Si)+(-0.537435*Fe_Si)+(0.431388*Feo_Si)+(-1.139286*Mn_Si)+(0.527984*Mg_Si)+(0.9884038*Ca_Si)+(-0.894467*Na_Si)+(0.16138688*K_Si)+(0.0778358*P_Si)-12.34961873
  DF2_INT5=(-0.40691*Ti_Si)+(2.60576*Al_Si)+(0.1610669*Fe_Si)+(1.345967*Feo_Si)+(0.4457959*Mn_Si)+(-0.260127*Mg_Si)+(-0.464534*Ca_Si)+(0.9211739*Na_Si)+(-1.2769499*K_Si)+(-0.142884*P_Si)+3.501318155
  #### Funcion IA+CA-CR+OI-Col ##
  x1=c(0.42744,-0.67554,8,-0.67554,-8,-0.67554)
  y1=c(-8,0.27663,5.53331,0.27663,4.73569,0.27663)
  plot(DF1_INT1,DF2_INT1,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(5,-2,'IA(90%)+CA(79.3%)')
  text(-4,-4,'CR(71.6%)+OI(96.4%)')
  text(-1,5,'Col(86.8%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-CR+OI ##
  x1=c(8,-0.63205,-1.50230,-0.63205,-2.73408,-0.63205)
  y1=c(0.76690,0.08764,-8,0.08764,8,0.08764)
  plot(DF1_INT2,DF2_INT2,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(5,-2,'IA(71.3%)')
  text(-4,2,'CR(76.6%)+OI(93.9%)')
  text(4,4,'CA(72.5%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-Col ##
  x1=c(8,-0.71170,-1.18110,-0.71170,-3.55140,-0.71170)
  y1=c(-3.06676,0.24138,8,0.24138,-8,0.24138)
  plot(DF1_INT3,DF2_INT3,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,0,'Col(85.5%)')
  text(4,-4,'CA(69.1%)')
  text(5,4,'IA(69.1%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CR+OI-Col ##
  x1=c(0.6676,-0.44102,8,-0.44102,-8,-0.44102)
  y1=c(-8,0.17933,6.27226,0.17933,4.24657,0.17933)
  plot(DF1_INT4,DF2_INT4,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,-3,'CR(75%)+OI(94.9%)')
  text(5,-3,'IA(89%)')
  text(-1,5,'Col(86.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion CA-CR+OI-Col ##
  x1=c(-3.42497,-0.033967,8,-0.033967,-4.17272,-0.033967)
  y1=c(8,-0.10997,-0.16286,-0.10997,-8,-0.10997)
  plot(DF1_INT5,DF2_INT5,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,-2,'CR(71.6%)+OI(96%)')
  text(5,-4,'Col(85.5%)')
  text(4,4,'CA(80%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  ##########2############
  #### Relaciones logaritmicas 2 ##
  Mg_Ti=log(D[,9]/D[,4])
  P_Ti=log(D[,13]/D[,4])
  Nb_Ti=log(Nb/D[,4])
  Ni_Ti=log(Ni/D[,4])
  V_Ti=log(V/D[,4])
  Y_Ti=log(Y/D[,4])
  Zr_Ti=log(Zr/D[,4])
  #### Funcion IA+CA-CR+OI-Col##
  DF1_INT6=(1.02293*Mg_Ti)+(0.63053*P_Ti)+(-0.93889*Nb_Ti)+(-0.41538*Ni_Ti)+(1.676898*V_Ti)+(0.453813*Y_Ti)+(0.5831823*Zr_Ti)+1.900726416
  DF2_INT6=(0.248529*Mg_Ti)+(-0.477177*P_Ti)+(-0.33628*Nb_Ti)+(-0.131072*Ni_Ti)+(-1.712035*V_Ti)+(0.213840*Y_Ti)+(-2.008435*Zr_Ti)-18.63750138
  #### Funcion IA-CA-CR+OI ##
  DF1_INT7=(0.8750597*Mg_Ti)+(0.4279822*P_Ti)+(-0.6864967*Nb_Ti)+(-0.372419*Ni_Ti)+(1.924254*V_Ti)+(0.835240*Y_Ti)+(0.8428416*Zr_Ti)+8.228368089
  DF2_INT7=(-1.171625*Mg_Ti)+(-2.650912*P_Ti)+(0.176065*Nb_Ti)+(0.1183849*Ni_Ti)+(-0.18532798*V_Ti)+(1.9213464*Y_Ti)+(0.3868149*Zr_Ti)+12.45160186
  #### Funcion IA-CA-Col ##
  DF1_INT8=(-0.801371*Mg_Ti)+(0.125028*P_Ti)+(0.908386*Nb_Ti)+(0.320442*Ni_Ti)+(-0.3683636*V_Ti)+(-0.6405805*Y_Ti)+(0.72337227*Zr_Ti)+8.1087217398
  DF2_INT8=(1.317201*Mg_Ti)+(2.199955*P_Ti)+(-0.1235449*Nb_Ti)+(-0.1339018*Ni_Ti)+(-0.8720114*V_Ti)+(-1.7825807*Y_Ti)+(-1.36498299*Zr_Ti)-20.63036447
  #### Funcion IA-CR+OI-Col ##
  DF1_INT9=(-0.85601*Mg_Ti)+(-0.300589*P_Ti)+(0.861909*Nb_Ti)+(0.384727*Ni_Ti)+(-1.5827037*V_Ti)+(-0.757282*Y_Ti)+(-0.692422*Zr_Ti)-4.468550646
  DF2_INT9=(0.21504*Mg_Ti)+(-0.503675*P_Ti)+(-0.32252*Nb_Ti)+(-0.122383*Ni_Ti)+(-1.7097486*V_Ti)+(0.426039*Y_Ti)+(-1.980676*Zr_Ti)-17.04082095
  #### Funcion CA-CR+OI-Col ##
  DF1_INT10=(-1.25554*Mg_Ti)+(-1.082014*P_Ti)+(1.437934*Nb_Ti)+(0.5454469*Ni_Ti)+(-1.6196297*V_Ti)+(0.3368725*Y_Ti)+(-0.71359906*Zr_Ti)+5.752160917
  DF2_INT10=(-0.02400*Mg_Ti)+(-0.054413*P_Ti)+(-0.8608025*Nb_Ti)+(-0.174160*Ni_Ti)+(-1.6407186*V_Ti)+(0.068523*Y_Ti)+(-1.772088*Zr_Ti)-21.02758313
  #### Funcion IA+CA-CR+OI-Col ##
  x1=c(0.92190,-0.82858,6.39297,-0.82858,-8,-0.82858)
  y1=c(8,0.29965,-8,0.29965,-4.20284,0.29965)
  plot(DF1_INT6,DF2_INT6,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,4,'CR(72.9%)+OI(100%)')
  text(-4,-6,'Col(90.2%)')
  text(5,-2,'IA(86.3%)+CA(88.5%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-CR+OI ##
  x1=c(8,-0.95018,-1.24490,-0.95018,-3.41007,-0.95018)
  y1=c(-3.76290,0.45941,8,0.45941,-8,0.45941)
  plot(DF1_INT7,DF2_INT7,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,-4,'CR(79.2%)+OI(100%)')
  text(2,-5,'CA(81.8%)')
  text(5,5,'IA(70.4%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-Col ##
  x1=c(-8,0.60491,0.95093,0.60491,4.00195,0.60491)
  y1=c(3.71126,-0.23211,-8,-0.23211,8,-0.23211)
  plot(DF1_INT8,DF2_INT8,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,-2,'IA(62.8%)')
  text(-3,5,'CA(76.2%)')
  text(5,3,'CoL(92.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CR+OI-Col ##
  x1=c(-0.87616,0.62149,-6.61289,0.62149,8,0.62149)
  y1=c(8,0.34939,-8,0.34939,-4.51524,0.34939)
  plot(DF1_INT9,DF2_INT9,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,0,'IA(85.5%)')
  text(0,-6,'Col(90.2%)')
  text(5,3,'CR(74.2%)+OI(98.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion CA-CR+OI-Col ##
  x1=c(-1.16430,-0.028516,-7.33632,-0.028516,8,-0.028516)
  y1=c(8,0.35743,-8,0.35743,-3.84452,0.35743)
  plot(DF1_INT10,DF2_INT10,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-6,0,'CA(94.7%)')
  text(0,-5,'Col(90.8%)')
  text(5,3,'CR(72.9%)+OI(98.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #######3##############
  #### Relaciones logaritmicas 3 ##
  La_Yb=log(D[,14]/D[,15])
  Ce_Yb=log(D[,23]/D[,15])
  Sm_Yb=log(D[,16]/D[,15])
  Nb_Yb=log(D[,18]/D[,15])
  Th_Yb=log(D[,17]/D[,15])
  Y_Yb=log(D[,21]/D[,15])
  Zr_Yb=log(D[,22]/D[,15])
  #### Funcion IA+CA-CR+OI-Col ##
  DF1_INT11=(-0.1672589*La_Yb)+(-1.2542899*Ce_Yb)+(1.295171*Sm_Yb)+(1.3318361*Nb_Yb)+(0.2698636*Th_Yb)+(1.9286976*Y_Yb)+(0.18097357*Zr_Yb)-3.815745639
  DF2_INT11=(-0.2426713*La_Yb)+(1.7265475*Ce_Yb)+(0.490224*Sm_Yb)+(-1.2755648*Nb_Yb)+(0.9602491*Th_Yb)+(0.8511852*Y_Yb)+(-0.4894082*Zr_Yb)-3.305510646
  #### Funcion IA-CA-CR+OI ##
  DF1_INT12=(0.0178001*La_Yb)+(-1.2689712*Ce_Yb)+(1.7407108*Sm_Yb)+(1.324421438*Nb_Yb)+(0.0288819*Th_Yb)+(1.580888497*Y_Yb)+(0.17161461*Zr_Yb)-3.3845534709
  DF2_INT12=(-2.099551*La_Yb)+(-2.044178*Ce_Yb)+(-0.41179008*Sm_Yb)+(1.022466699*Nb_Yb)+(1.24448424*Th_Yb)+(1.87700276*Y_Yb)+(1.07017399797*Zr_Yb)-0.2920468400
  #### Funcion IA-CA-Col ##
  DF1_INT13=(0.092724*La_Yb)+(0.752143*Ce_Yb)+(0.9296053*Sm_Yb)+(0.12351021*Nb_Yb)+(0.3479451*Th_Yb)+(1.472513*Y_Yb)+(-0.0339674*Zr_Yb)-5.801482381
  DF2_INT13=(-2.038286*La_Yb)+(-0.073322*Ce_Yb)+(-1.360432*Sm_Yb)+(-0.0782899*Nb_Yb)+(1.8248761*Th_Yb)+(2.7738488*Y_Yb)+(0.44440139*Zr_Yb)-3.684349292
  #### Funcion IA-CR+OI-Col ##
  DF1_INT14=(0.720851*La_Yb)+(-1.352147*Ce_Yb)+(1.378563*Sm_Yb)+(1.1641465*Nb_Yb)+(-0.0423769*Th_Yb)+(1.5584709*Y_Yb)+(-0.1644980*Zr_Yb)-2.9336489118
  DF2_INT14=(0.2378909*La_Yb)+(-2.03548886*Ce_Yb)+(-0.2501036699*Sm_Yb)+(1.34733326*Nb_Yb)+(-0.760673982*Th_Yb)+(-0.786605747*Y_Yb)+(0.37736968328*Zr_Yb)+4.154732286
  #### Funcion CA-CR+OI-Col ##
  DF1_INT15=(-0.977026*La_Yb)+(-1.3886489*Ce_Yb)+(1.36560*Sm_Yb)+(1.8999127*Nb_Yb)+(0.5690460*Th_Yb)+(1.65772638*Y_Yb)+(-0.30523813*Zr_Yb)-0.87680549008
  DF2_INT15=(-0.086967*La_Yb)+(1.1636159*Ce_Yb)+(0.3635930*Sm_Yb)+(-0.90127239*Nb_Yb)+(1.1257989*Th_Yb)+(1.19149068*Y_Yb)+(-0.39964298*Zr_Yb)-3.915383182
  #### Funcion IA+CA-CR+OI-Col ##
  x1=c(-0.69292,0.64148,-6.91145,0.64148,8,0.64148)
  y1=c(-8,0.34301,8,0.34301,3.04640,0.34301)
  plot(DF1_INT11,DF2_INT11,pch=2,col='red',xlab=c(expression(bold(DF1(IA+CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA+CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,1,'IA(91.4%)+CA(90.4%)')
  text(5,-3,'CR(74.3%)+OI(100%)')
  text(4,6,'Col(81%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-CR+OI ##
  x1=c(-8,0.58959,0.87619,0.58959,3.67939,0.58959)
  y1=c(-5.45793,0.68699,8,0.68699,-8,0.68699)
  plot(DF1_INT12,DF2_INT12,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-CR+OI)))),ylab=c(expression(bold(DF2(IA-CA-CR+OI)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,3,'IA(75.7%)')
  text(-3,-5,'CA(65.8%)')
  text(6,-4,'CR(80.5%)+OI(100%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CA-Col ##
  x1=c(-8,0.90473,0.64537,0.90473,4.86730,0.90473)
  y1=c(-7.28196,0.82230,8,0.82230,-8,0.82230)
  plot(DF1_INT13,DF2_INT13,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CA-Col)))),ylab=c(expression(bold(DF2(IA-CA-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-5,2,'IA(72.7%)')
  text(2,-5,'CA(64.5%)')
  text(6,-5,'Col(84%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion IA-CR+OI-Col ##
  x1=c(-0.87235,0.37157,-6.10890,0.37157,8,0.37157)
  y1=c(8,-0.26385,-8,-0.26385,-2.82217,-0.26385)
  plot(DF1_INT14,DF2_INT14,pch=2,col='red',xlab=c(expression(bold(DF1(IA-CR+OI-Col)))),ylab=c(expression(bold(DF2(IA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-6,0,'IA(90.3%)')
  text(5,3,'CR(74.7%)+OI(100%)')
  text(0,-5,'Col(84.7%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
  #### Funcion CA-CR+OI-Col ##
  x1=c(-0.10284,-0.15459,-8,-0.15459,8,-0.15459)
  y1=c(-8,0.29462,5.41425,0.29462,4.74335,0.29462)
  plot(DF1_INT15,DF2_INT15,pch=2,col='red',xlab=c(expression(bold(DF1(CA-CR+OI-Col)))),ylab=c(expression(bold(DF2(CA-CR+OI-Col)))),xlim=c(-8,8),ylim=c(-8,8),axes=F);
  text(-4,-3,'CA(95.7%)')
  text(5,-3,'CR(74.3%)+OI(94.1%)')
  text(2,6,'Col(81%)')
  rect(-8,-8,8,8);
  axis(1, at=seq(-8,8,by=2),labels=T);
  axis(2, at=seq(-8,8,by=2),labels=T);
  lines(x1,y1,lwd=2);
}
B=readline('Did you want to generate a report with the DF and log ratios values? Yes=(Y),No=(N)')
if (B=='Y'){
  X=D[,2]
  x=D[,1] ### Colocar la posici?n de la muestra
  fa=data.frame(D,X,Ti_Si,Al_Si,Fe_Si,Feo_Si,Mn_Si,Mg_Si,Ca_Si,Na_Si,K_Si,P_Si,X,Mg_Ti,P_Ti,Nb_Ti,Ni_Ti,V_Ti,Y_Ti,Zr_Ti,X,La_Yb,Ce_Yb,Sm_Yb,Nb_Yb,Th_Yb,Y_Yb,Zr_Yb,X,DF1_INT1,DF2_INT1,DF1_INT2,DF2_INT2,DF1_INT3,DF2_INT3,DF1_INT4,DF2_INT4,DF1_INT5,DF2_INT5,DF1_INT6,DF2_INT6,DF1_INT7,DF2_INT7,DF1_INT8,DF2_INT8,DF1_INT9,DF2_INT9,DF1_INT10,DF2_INT10,DF1_INT11,DF2_INT11,DF1_INT12,DF2_INT12,DF1_INT13,DF2_INT13,DF1_INT14,DF2_INT14,DF1_INT15,DF2_INT15)
  write.csv(fa,file='Report_Verma_2013_int.csv',row.names=T)
}else if (B=='N'){
  b=print('Thanks, I invite you to generate more diagrams at www.platicandodelatierra.com ')
}