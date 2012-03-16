##########################################
##########################################
## Some useful scripts able to handle file from
## the last version of acacia-nrg 
## (available at "git@gitorious.org:acacia-coop/acacia-coop.git")
##########################################
##########################################

trecol=c()
trecol["kind"]="chartreuse2"
trecol["real"]="#5613CA"
trecol["self"]="#FF9400"

## Read a file in the good csv format and return a table
readAcaciaOutput<-function(filename){
	return( read.csv(filename,sep="\t",skip=26) )
	
}

##################################################

#specific plot two see vital space density function
#for each number of obstacles
threeVitalSpacePlot<-function(data,...){
	
	library(sm);
	
# plot densities;
	sm.density.compare(data$vital.space, data$no, xlab="Vital Space");
	title(main="Vital Space wrt Obstacle Number");
	
# add legend via mouse click;
	colfill<-c(2:(2+length(unique(data$no))));
	legend("topright",legend=unique(sort(data$no)), fill=colfill) ;
}



interPlotObstacleEffect<-function(){interaction.plot(kind$fbs,kind$no,kind$n_alive,main="Mean alive agents at the end",xlab="Available Ressource (nf by timestep)",ylab="Number of alive agents")}




#onEnv<-function(a,nf,rs,no){return(a[a$Num_Fruits == nf & a$Nb.Obstacle == no & a$Reg.speed == rs,])}

############################################
## oneEnv return data from on set of environmental setup
oneEnv<-function(aData,nf,rs,no){return(aData[aData$nf == nf & aData$no == no & aData$rs == rs,])}


diffbyrapCol<-function(a,t){
c=0
allcol=rampPalette(length(unique(a$Nb.Obstacle))*length(unique(a$Num_Fruits))*length(unique(a$Reg.speed)))
print(allcol)
rap=a$Fq_kind[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]/a$Fq_selfish[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" ]
   
 plot(a$Fq_kind[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"] - a$Fq_selfish[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]~rap,type="n")
for ( i in unique(a$Num_Fruits)){
  for (j in unique(a$Reg.speed)){
    
    for (k in unique(a$Nb.Obstacle)){
    
    rap=a$Fq_kind[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Nb.Obstacle == k & a$Num_Fruits == i & a$Reg.speed == j]/a$Fq_selfish[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Nb.Obstacle == k & a$Num_Fruits == i & a$Reg.speed == j]
   
   points(a$Fq_kind[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Nb.Obstacle == k & a$Num_Fruits == i & a$Reg.speed == j] - a$Fq_selfish[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"& a$Nb.Obstacle == k & a$Num_Fruits == i & a$Reg.speed == j]~rap,col=allcol[c])
    c=c+1
    print(c)
  }
 }
}
}

#tester ça mais sans les "1/3"
creatMat<-function(a,t){
   res=c()
    for(i in unique(a$Num_Fruits)){
	test= a$Num_Fruits == i & a$Counter == t
	res=rbind(res,tapply(a$Fq_kind[test]-a$Fq_selfish[test],a$Reg.speed[test],mean))  
   }
return(res)
}

krs<-function(a){
 plot(a$Fq_realist~a$Counter,col=trecol["real"],ylim=c(0,100))
 points(a$Fq_selfish~a$Counter,col=trecol["self"])
 points(a$Fq_kind~a$Counter,col=trecol["kind"])
}

krsF<-function(a){
#a$Fq_realist=a$Fq_realist/a$Fq_alive
#a$Fq_kind=a$Fq_kind/a$Fq_alive
#a$Fq_selfish=a$Fq_selfish/a$Fq_alive

 plot(tapply( a$Fq_realist/a$Fq_alive,a$Counter,mean),type="l",col=trecol["real"],ylim=c(0,1))
 points(tapply( a$Fq_selfish/a$Fq_alive,a$Counter,mean),type="l",col=trecol["self"])
 points(tapply( a$Fq_kind/a$Fq_alive,a$Counter,mean),type="l",col=trecol["kind"])
}


krsFM<-function(a){
#a$Fq_realist=a$Fq_realist/a$Fq_alive
#a$Fq_kind=a$Fq_kind/a$Fq_alive
#a$Fq_selfish=a$Fq_selfish/a$Fq_alive

  plotmeans(a$Fq_realist/a$Fq_alive~a$Counter,type="l",col=trecol["real"],ylim=c(0,1),n.label=F,barcol="#16003D")
  par(new=T) 
  plotmeans(a$Fq_selfish/a$Fq_alive~a$Counter,type="l",col=trecol["self"],ylim=c(0,1),n.label=F,barcol="#16003D")
  par(new=T)
  plotmeans(a$Fq_kind/a$Fq_alive~a$Counter,type="l",col=trecol["kind"],ylim=c(0,1),n.label=F,barcol="#16003D")
  
  legend("topleft",c("kind","TFT","selfish"),col=c(trecol["kind"],trecol["real"],trecol["self"]),lty=1)
  
    par(new=F)

}


AvsVar<-function(a,time){
plot(tapply( a$Fq_alive[ a$Obstacle.Adapt == "false" & a$Counter == time],a$attention.angle[ a$Obstacle.Adapt == "false" & a$Counter == time],mean),type="l",col="black",ylim=c(min(a$Fq_alive[a$Counter == time]),max(a$Fq_alive[a$Counter == time])))
par(new=T)
plot(tapply( a$Fq_alive[ a$Obstacle.Adapt == "true" & a$Counter == time],a$attention.angle[ a$Obstacle.Adapt == "true" & a$Counter == time],mean),type="l",col="light #FF9400",ylim=c(min(a$Fq_alive[a$Counter == time]),max(a$Fq_alive[a$Counter == time])))
}

#library(gplots)
alive_initBehav<-function(a){
  plot(a$Fq_alive[a$Ag.Initial.Behav == "kind"] ~ a$Counter[a$Ag.Initial.Behav == "kind"],col=trecol["kind"],ylim=c(0,100))
  points(a$Fq_alive[a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"] ~ a$Counter[a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"],col=trecol["real"])
  points(a$Fq_alive[a$Ag.Initial.Behav == "selfish"] ~ a$Counter[a$Ag.Initial.Behav == "selfish"],col=trecol["self"])
}

alive_initBehavM<-function(a){
    par(ann=F,lwd=4,par(mar=c(2,4,4,0)))
  plotmeans(a$Fq_alive[a$Ag.Initial.Behav == "kind"] ~ a$Counter[a$Ag.Initial.Behav == "kind"],col=trecol["kind"],ylim=c(0,100),n.label=F,barcol="#16003D" )
  par(new=T)
  plotmeans(a$Fq_alive[a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"] ~ a$Counter[a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"],col=trecol["real"],ylim=c(0,100),n.label=F ,barcol="#16003D")
  par(new=T)
  plotmeans(a$Fq_alive[a$Ag.Initial.Behav == "selfish"] ~ a$Counter[a$Ag.Initial.Behav == "selfish"],col=trecol["self"],ylim=c(0,100),n.label=F,barcol="#16003D" )
  
  legend("bottomleft",c("kind only","mixte","selfish only"),col=c(trecol["kind"],trecol["real"],trecol["self"]),lty=1)
#   par(new=T)
#   plotmeans(a$Fq_alive[a$Ag.Initial.Behav == "kind_self"] ~ a$Counter[a$Ag.Initial.Behav == "kind_self"],col="#561300",ylim=c(0,100),n.label=F,barcol="#16003D" )
 title(main="Nombre d'agents survivants moyen\n en fonction de la population d'origine et du temps",ylab="Nombre d'agents survivants")

  par(new=F)

}

alive_initBehavP<-function(a,nf,rs,no){alive_initBehavM(a[ a$Num_Fruits == nf & a$Reg.speed == rs & a$Nb.Obstacle == no,] )}

alive_krsM<-function(a,nf,rs,no){

  par(mfrow=c(1,2),ann=F,oma=c(0,0,2,0),lwd=4)
  alive_initBehavM(a[ a$Num_Fruits == nf & a$Reg.speed == rs & a$Nb.Obstacle == no,] )
#  legend(c("kind only","selfish only","1/3 de chaque"),col=c(trecol["kind"],trecol["self"],trecol["real"]))
 
 krsFM(a[ a$Num_Fruits == nf & a$Reg.speed == rs & a$Nb.Obstacle == no & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish",] )  
title(main="Répartition des trois comportements\n de la population mixte en fonction du temps",ylab="Pourcentage d'individus")
 

title(paste("Nb agents en fonction du type de pop avec FBS=",rsToFbs(nf,rs),"et NO=",no,"dans une population mixte"),outer=T)

par(mfrow=c(1,1),oma=c(0,0,5,0))

}

alive_krs<-function(a,nf,rs,no){
  par(mfrow=c(1,2),ann=F,oma=c(0,0,5,0))
 alive_initBehavM(a[ a$Num_Fruits == nf & a$Reg.speed == rs & a$Nb.Obstacle == no,] )
 
 krs(a[ a$Num_Fruits == nf & a$Reg.speed == rs & a$Nb.Obstacle == no & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish",] )
 title(paste("Analyse du taux de survie des agents en fonction de la population de départ pour\n",rsToFbs(nf,rs),"fruits par unité de temps et",no,"obstacles"),outer=T)
par(mfrow=c(1,1))
}

diffbyrapI<-function(a,t){
  rap=a$Fq_kind[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]/a$Fq_selfish[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]
  plot(a$Fq_kind[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"] - a$Fq_selfish[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]~rap)
  

}


  diffbyrap<-function(a,t){
  rap=a$Fq_kind[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]/a$Fq_selfish[ a$Counter == 0 & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]
# print("-----")

# diff=tapply(mix$Fq_alive[ mix$Counter == 1000 & mix$Ag.Initial.Behav == "kind"]) - mix$Fq_alive[ mix$Counter == 1000 & mix$Ag.Initial.Behav == "selfish"]

diff=a$Fq_kind[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"] - a$Fq_selfish[ a$Counter == t & a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish"]
# print(diff[diff>0])
# print("-----")
  plot(diff~rap,type="n")
  points(diff[diff>=0]~rap[diff >= 0],col=trecol["kind"])
  points(diff[diff<=0]~rap[diff <= 0],col=trecol["self"])
   legend("topleft",c("kind > selfish","selfish > kind"),col=c(trecol["kind"],trecol["self"]),lty=1)

  
  }

  diffby100<-function(a,t){

diff=a$Fq_kind[ a$Counter == 1000 & a$Ag.Initial.Behav == "kind"] - a$Fq_selfish[ a$Counter == 1000 & a$Ag.Initial.Behav == "selfish"]
cent=(a$Fq_alive[ a$Counter == 1000 & a$Ag.Initial.Behav == "kind"] + a$Fq_alive[ a$Counter == 1000 & a$Ag.Initial.Behav == "selfish"])/2

diff=a$Fq_kind[ a$Counter == t & a$Ag.Initial.Behav == "kind"] - a$Fq_selfish[ a$Counter == t & a$Ag.Initial.Behav == "selfish"]
# print(diff[diff>0])
# print("-----")
  plot(diff~cent,type="n")
  points(diff[diff>=0]~cent[diff >= 0],col=trecol["kind"])
  points(diff[diff<=0]~cent[diff <= 0],col=trecol["self"])
  legend("topleft",c("kind only > selfish only","selfish > kind only"),col=c(trecol["kind"],trecol["self"]),lty=1)
#    plot(diff~cent,type="n")
  }

twodiff<-function(a,nf,rs,no){
par(mfrow=c(1,2),ann=F,oma=c(0,0,2,0),lwd=4)

diff=a$Fq_kind[a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Num_Fruits == nf & a$Nb.Obstacle == no & a$Reg.speed == rs] - a$Fq_selfish[ a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Num_Fruits == nf & a$Nb.Obstacle == no & a$Reg.speed == rs]
 
 plot(diff~a$Counter[ a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Num_Fruits == nf & a$Nb.Obstacle == no & a$Reg.speed == rs],type="n")

  points(diff[ diff > 0 ]~a$Counter[a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Num_Fruits == nf & a$Nb.Obstacle == no & a$Reg.speed == rs][diff>0],col=trecol["kind"])

  points(diff[diff<0]~a$Counter[a$Ag.Initial.Behav == "1/3kind1/3realist1/3selfish" & a$Num_Fruits == nf & a$Nb.Obstacle == no & a$Reg.speed == rs][diff<0],col=trecol["self"])
#     legend("topleft",c("kind only > selfish only","selfish > kind only"),col=c(trecol["kind"],trecol["self"]),lty=1)

  title(main="Différence en fonction du temps")
  diffbyrap(a[ a$Num_Fruits == nf & a$Nb.Obstacle == no & a$Reg.speed == rs,],1000)
  title(main="Différence au temps 1000\n en fonction du rapport kind/selfish au départ")
title(paste("Analyse N_kind-N_selfish pour FBS=",rsToFbs(nf,rs),"et NO=",no,"dans une population mixte"),outer=T)
par(mfrow=c(1,1))
# hist(diff[diff<0])
}
acacia.test<-function(a,nf,rs,no,t){
t.test(acacia(a[ a$Ag.Initial.Behav == "kind" & a$Counter == t,],nf,rs,no)$Fq_kind,acacia(a[ a$Ag.Initial.Behav == "selfish" & a$Counter == t,],nf,rs,no)$Fq_selfish)
}



pngA<-function(a,nf,rs,no,h,w){
 png(paste("initial",nf,"_",rs,"_",no,".png",sep=""),width=w,height=h,pointsize=40)
 alive_initBehavP(a,nf,rs,no)
 dev.off()
}


diff1000mean100<-function(a,d_time,m_time){
res=c()
for(i in unique(a$Nb.Obstacle)){
    for(j in unique(a$Reg.speed)){
	for(k in unique(a$Num_Fruits)){
	    diff=mean(a$Fq_alive[a$Nb.Obstacle == i & a$Reg.speed == j & a$Num_Fruits == k & a$Ag.Initial.Behav == "kind"& a$Counter == d_time] )-mean(a$Fq_alive[a$Nb.Obstacle == i & a$Reg.speed == j & a$Num_Fruits == k & a$Ag.Initial.Behav == "selfish"& a$Counter == d_time])
	    mean=(mean(a$Fq_alive[a$Nb.Obstacle == i & a$Reg.speed == j & a$Num_Fruits == k & a$Ag.Initial.Behav == "kind"& a$Counter == m_time])+mean(a$Fq_alive[a$Nb.Obstacle == i & a$Reg.speed == j & a$Num_Fruits == k & a$Ag.Initial.Behav == "selfish" & a$Counter == m_time]))/2
	    res=rbind(res,c(diff,mean))
	}
    }
	

}
return(res)    
}
graphAlive<-function(a,nf,rs,no){

w=2100
h=1100
    png(paste("alive_krs_",nf,"_",rs,"_",no,".png",sep=""),width=w,height=h,pointsize=35)
    
    alive_krsM(a,nf,rs,no)
     dev.off()

}

graphAliveAndBox<-function(a,nf,rs,no){

w=2100
h=1100
    png(paste("alive_",nf,"_",rs,"_",no,".png",sep=""),width=w,height=h,pointsize=35)
    
    alive_initBehavM(acaciab(a,nf,rs,no))
     dev.off()
     
    png(paste("aliveBox_",nf,"_",rs,"_",no,".png",sep=""),width=w,height=h,pointsize=35)
    
    alive_initBehavBox(acaciab(a,nf,rs,no))
     dev.off()

}

rsToFbs<-function(nf,rs){
   return(nf/(2*2^(6-rs)))
}

graphTwodiff<-function(a,nf,rs,no){
    
w=2100
h=1100 
    png(paste("twodiff_",nf,"_",rs,"_",no,".png",sep=""),width=w,height=h,pointsize=35)
    
   twodiff(a,nf,rs,no)
     dev.off()

}
# nf=c(5,11,17)
# rs=c(3,4,5)
# rs=2*2^(6-rs)

# fbs=c(nf[1]*rs,nf[2]*rs,nf[3]*rs)

#addFbs<-function(a){a$fbs = a$Num_Fruits/(2*2^(6-a$Reg.speed))}
#addFbs<-function(a){a$fbs = a$nf/(2*2^(6-a$rs))}
addFbs<-function(a){
	fbs=a$nf/(2*2^(6-a$rs));
	a=cbind(a,fbs);
	return(a);
}
addFbsChar<-function(a){
	fbsChar=paste("(",a$nf,",",a$rs,")",sep="");
	a=cbind(a,fbsChar);
	return(a);
}

# 
#  png("diff_fct_100.png",width=1500,height=1500,pointsize=35)
# par(ann=F,lwd=6)
# # plot(res[1]~res[2])
#  plot(res[,1]~res[,2])
#  
# abline(lm(res[,1]~res[,2]),col="red")
#  title(main="Différences entre le nombre d'individus\n au temps 1000 d'une population \"kind only\" vs \"selfish only\" \n en fonction du nombre d'individus au temps 100",ylab="Différence au temps 1000",xlab="Nombre d'individus au temps 100") 
#  dev.off()
# 
# interaction.plot( mix$Counter[ mix$Ag.Initial.Behav == "kind" & mix$Num_Fruits == 17 & mix$Nb.Obstacle == 30],mix$Reg.speed[ mix$Ag.Initial.Behav == "kind" & mix$Num_Fruits == 17 & mix$Nb.Obstacle == 30], mix$Fq_alive[ mix$Ag.Initial.Behav == "selfish" & mix$Num_Fruits == 17 & mix$Nb.Obstacle == 30])
#interaction.plot(neochrome$fbs[neochrome$Counter == 1000],neochrome$Ag.Initial.Behav[neochrome$Counter == 1000],neochrome$Fq_alive[neochrome$Counter == 1000],legend=F)
# neochrome$fbs = neochrome$Num_Fruits/(2*2^(6-neochrome$Reg.speed))
# 
# aov(neochrome$Fq_alive[neochrome$Counter == 1000] ~ as.factor(neochrome$Nb.Obstacle[neochrome$Counter == 1000])*as.factor(neochrome$Reg.speed[neochrome$Counter == 1000])*as.factor(neochrome$Num_Fruits[neochrome$Counter == 1000])*neochrome$Ag.Initial.Behav[neochrome$Counter == 1000])
# 
# aov(neochrome$Fq_alive[neochrome$Counter == 1000] ~ as.factor(neochrome$Nb.Obstacle[neochrome$Counter == 1000])*neochrome$Ag.Initial.Behav[neochrome$Counter == 1000])

#MAUVAIS
# speed=neochrome$Reg.speed
# population=neochrome$Ag.Initial.Behav
# nf=neochrome$Num_Fruits
# no=neochrome$Nb.Obstacle
# alive=neochrome$Fq_alive
# t1000 = neochrome$Counter == 1000

# POUR CHANGER LES NOM POUR PLUS DE LISIBILITé
# > names(newname)[28]="nf"
# > names(newname)[10]="no"
# > names(newname)[11]="alive"
# > names(newname)[9]="rs"
# > names(newname)[4]="pop"


#Pour faire ce parcours les noms doivent être changé cf ci-dessus
parcours<-function(a){
    
    allMeanSd=c()
    
    for (Reg_Speed in unique(a$rs)){
	
	for (Num_Fruits in unique(a$nf)){
	    
	    for (Num_Obs in unique(a$no)){
		
		#graphAlive(neochrome,Num_Fruits,Reg_Speed,Num_Obs)
		
		for (Initial_Pop in unique(a$init)){
		    cur=oneEnv(a,Num_Fruits,Reg_Speed,Num_Obs)[oneEnv(a,Num_Fruits,Reg_Speed,Num_Obs)$init == Initial_Pop,]
		    
# 		    print(cbind(Reg_Speed,as.array(tapply(cur$alive[cur$pop == Initial_Pop],cur$Counter[cur$pop == Initial_Pop],mean))))stringsAsFactors = FALSE’
# 		    print(cbind(Reg_Speed,Num_Fruits,Num_Obs,Initial_Pop,tapply(cur$alive[cur$pop == Initial_Pop],cur$Counter[cur$pop == Initial_Pop],mean)))
# 		    means = data.frame(cbind(Reg_Speed,Num_Fruits,Num_Obs,Initial_Pop),rbind(tapply(cur$alive[cur$pop == Initial_Pop],cur$Counter[cur$pop == Initial_Pop],mean)))
# 		    stde = data.frame(cbind(Reg_Speed,Num_Fruits,Num_Obs,Initial_Pop),rbind(tapply(cur$alive[cur$pop == Initial_Pop],cur$Counter[cur$pop == Initial_Pop],sd)))
# 		    rownames(means)="Moyenne"
# 		    rownames(stde)="Ecart Type"
		    
# 		    allMeanSd=rbind(allMeanSd,means,stde)
		    allMeanSd=rbind(allMeanSd,data.frame(cbind(Reg_Speed,Num_Fruits,Num_Obs,Initial_Pop,mean(cur$n_alive),sd(cur$n_alive))))

		    print(allMeanSd)
		}
	    }
	}
    }
    
    names(allMeanSd)[5]="mean"
    names(allMeanSd)[6]="standard deviation"
    return(allMeanSd)
}

#Pour faire ce parcours les noms doivent être changé cf ci-dessus
allttest<-function(a){
result=c()	
    for (rs in unique(a$rs)){
	for (nf in unique(a$nf)){
	    for (no in unique(a$no)){
		 r=t.test(oneEnv(a[a$init == "kind",],nf,rs,no)$n_alive,oneEnv(a[a$init == "self",],nf,rs,no)$n_alive)
		result=rbind(result,cbind(rs,nf,no,r$statistic,r$estimate[1],r$estimate[2],sd(oneEnv(a[a$init == "kind",],nf,rs,no)$n_alive),sd(oneEnv(a[a$init == "self",],nf,rs,no)$n_alive)))
		print(paste(rs,nf,no))
	    }
	}
    }
colname=c("g_speed","nf","no","T.value","Mean Altruist","Mean Selfish","SD Altruist","SD Selfish")
colnames(result)=colname
return(result)
}

#take value frome different csv file and concatenate them in one table
dataProcess<-function(){

selfA = readAcaciaOutput("data/08:47:37.874 PM 15-févr.-2012ACACIAout.csv")
selfB = readAcaciaOutput("data/08:47:50.666 PM 15-févr.-2012ACACIAout.csv")
kindA = readAcaciaOutput("data/09:50:37.407 PM 14-févr.-2012ACACIAout.csv")
kindB = readAcaciaOutput("data/10:16:24.087 PM 14-févr.-2012ACACIAout.csv")

kind=rbind(kindA,kindB)
self=rbind(selfA,selfB)
kind=kind[kind$time==2000,]
self=self[self$time==2000,]
self=addFbs(self)
kind=addFbs(kind)

self=addFbsChar(self)
kind=addFbsChar(kind)
init="self"
self=cbind(self,init)
init="kind"
kind=cbind(kind,init)
}
#########reminder
fixAParam<-function(data,param,val){
	return(data[param == val,])
}

aliveWRTrs<-function(){
	for( nf in unique(kind$nf)){
		png(paste("alive_agent_wrt_rs-NF",nf,"-NO20.png",sep=""))
		currA=kind[kind$nf==nf & kind$no == 20,]
		currS=self[self$nf==nf & self$no == 20,]
		barplotAcacia(currA,currS,"rs","n_alive",xlab="Regeneration Speed",ylab="Agents Alive",ylim=c(0,100))
		dev.off()
	}
	
        

	
}

tableMaker<-function(){

	myEnvA=kind[kind$fbs==0.6875,]#2.125
	myEnvS=self[self$fbs==0.6875,]#
	
	M_a=tapply(myEnvA$n_alive,myEnvA$no,mean)
	M_s=tapply(myEnvS$n_alive,myEnvS$no,mean)
	
	SD_a=tapply(myEnvA$n_alive,myEnvA$no,sd)
	SD_s=tapply(myEnvS$n_alive,myEnvS$no,sd)
	
	cbind(M_a,M_s,SD_a,SD_s)

}
barplotAcacia<-function(a,b,x,y,fun=mean,minus=F,...){
	Altruistic=tapply( a[,y], a[,x], fun);
	Selfish=tapply( b[,y], b[,x], fun);
	both=rbind(Altruistic,Selfish);
	AltruisticSe=tapply( a[,y], a[,x], sd);
	SelfishSe=tapply( b[,y], b[,x], sd);
	bothSe=rbind(AltruisticSe,SelfishSe);
	if(minus){
		d=Altruistic-Selfish	
		dSe=AltruisticSe - SelfishSe
		barplot2(d,beside=T,...);
	}
	else
		barplot2(both,beside=T,legend=T,plot.ci=T,ylim=c(0,100),ci.u=both+bothSe,ci.l=both,col=c("white","grey"),...);
}



alive_initBehavBox<-function(a){
    par(ann=F,lwd=4,par(mar=c(2,4,4,0)))
  boxplot(a$alive[a$pop == "kind"] ~ a$Counter[a$pop == "kind"],col=trecol["kind"],ylim=c(0,100),n.label=F,barcol="#16003D" )
  par(new=T)
  boxplot(a$alive[a$pop == "1/3kind1/3realist1/3selfish"] ~ a$Counter[a$pop == "1/3kind1/3realist1/3selfish"],col=trecol["real"],ylim=c(0,100),n.label=F ,barcol="#16003D")
  par(new=T)
  boxplot(a$alive[a$pop == "selfish"] ~ a$Counter[a$pop == "selfish"],col=trecol["self"],ylim=c(0,100),n.label=F,barcol="#16003D" )
  
  legend("bottomleft",c("kind only","mixte","selfish only"),col=c(trecol["kind"],trecol["real"],trecol["self"]),lty=1)
#   par(new=T)
#   boxplot(a$alive[a$pop == "kind_self"] ~ a$Counter[a$pop == "kind_self"],col="#561300",ylim=c(0,100),n.label=F,barcol="#16003D" )
 title(main="Nombre d'agents survivants moyen\n en fonction de la population d'origine et du temps",ylab="Nombre d'agents survivants")

  par(new=F)

}


blanc<-function(){
un=read.csv("rs3_nf5-17_no10-30.txt",sep="\t",header=T)
#    dos=read.csv("rs4-5_nf5-17_no10-20.txt,sep="\t",header=T)
    dos=read.csv("rs4-5_nf5-17_no10-20.txt",sep="\t",header=T)
tres=read.csv("rs4-5_nf5-17_no30.txt",sep="\t",header=T)
 un=changeColName(un)
un=addFbs(un)
 dos=changeColName(dos)
dos=addFbs(dos)

 tres=changeColName(tres)
tres=addFbs(tres)
 quatro=newname[ newname$pop == "selfish" & newname$Counter == 1500,]
 un=un[ un$Counter == 1500 ,]
 dos=dos[ dos$Counter == 1500 ,]
 tres=tres[ tres$Counter == 1500 ,]
 
quatrob=c()
for(no in unique(quatro$no)){
    for (nf in unique(quatro$nf)){
	for (rs in unique(quatro$rs))
	    quatrob=rbind(quatrob,acaciab(quatro,nf,rs,no)[1:100,])
	
	}
    }


quatro=quatrob

all_1500_new=rbind(un,dos,tres,quatro)
}
#######################################################
# New function to use with the latest version of
# ACACIACoop
##
#######################################################


##Function non generique spécifique exp 5000Ts kind vs self
plotAliveTwoPop<-function(data,col=c("red","white")){

 boxplot(data$n_alive[0:20200] ~ data$time[0:20200],ylim=c(0,100),outline=F,col=col[1])
 par(new=T)
 boxplot(data$n_alive[20201:40400] ~ data$time[20201:40400],ylim=c(0,100),outline=F,col=col[2])

}


##################################################
#interaction plot ressource/alive for three NO
ThreIntPlot<-function(data){
par(mfrow=c(3,1),mar=c(4,4,2,2))
	for(no in unique(data$no)){
		interaction.plot(data$fbs[data$no == no],data$init[data$no == no],data$n_alive[data$no == no])
	}
}

#####
## quick and dirty copy past from medea scripts
#####
plotTwoHeatMat<-function(data,env,mod){
	layout(mat=matrix(c(1,2),nrow=2,ncol=1),heights=matrix(c(.5,.5),nrow=1,ncol=2))
	
	par(mar=c(0.6,5.1,5.1,2))
	data[,1]=data[,1]-data[,1]%%mod
	data[,2]=data[,2]-data[,2]%%mod
	
	resul=createHeatMat("V4","V1",as.data.frame(data[data[,5]==env,]),mod=mod)
	#resul=round(resul,20);
	xlab=expression(available(Q[r1]))
	ylab="#active agents"
	cols=c("white",blue)
	yaxs=seq(0,100,20)
	printASlice(resul,expression(available(Q[r1])),expression(harvest(Q[r1])),c(0.45,1.0),c(-5.50,105.5),cols=cols,axes=FALSE)
axis(2,yaxs)
	box()
	#image(resul,c(0.45,1.0),c(-0.50,100.5),xaxt="n")
	par(mar=c(5.1,5.1,.5,2))
	resul=createHeatMat("V4","V2",as.data.frame(data[data[,5]==env,]),mod=mod)
	#resul=round(resul,20);
	yaxs=colnames(resul)
	resul=resul[,as.character(sort(as.numeric(colnames(resul)),decreasing=T))]
	colnames(resul)=yaxs
	cols=c("white",red)
#	printASlice(resul,expression(available(Q[r1])),expression(harvest(Q[r0])),c(0.45,1.0),c(-5.50,100.5),cols=cols)
	printASlice(resul,expression(available(Q[r1])),expression(harvest(Q[r0])),c(0.45,1.0),c(-5.5,105.5),cols=c("white",red),axes=FALSE)
	axis(2,seq(100,0,-20),at=seq(-0,100,20))
	axis(1,seq(.5,.95,.1))
	box()

}

createHeatMat<-function(x,y,data,complete=TRUE,mod=1,ymin=0,ymax=100){
 	res=daply(.variables=c(x,y),.data=data,.drop_i=FALSE,.fun=function(x)length(x[,1]))
	res[is.na(res)]<-0
	res=res/apply(res,1,sum)
	#the following for loop will add missing column
	if(complete) {
		for(i in seq(ymin,ymax,mod)){ 
			if( !(as.character(i)%in% colnames(res))){
				res=cbind(res,0);
				colnames(res)[ncol(res)]=as.character(i);
			}
		}
		res=res[,as.character(sort(as.numeric(colnames(res))))] #Allow to sort the matrix by colname
	}



	return(res)	
}
####endofcopypaste

printGraph<-function(){
	
	for(env in(list( c(3,5),c(5,17),c(4,11)))){
		png(paste("alive_agent_wrNo0to200-NF",env[2],"-RS",env[1],".png",sep=""))
		barplotAcacia(kind[kind$nf == env[2]& kind$rs == env[1],], self[self$nf == env[2]& self$rs == env[1],],"no","n_alive",xlab="Number of Obstacles",ylab="Agents Alive",ylim=c(0,100),main=paste("Alive agent at t=200\nfor nf=",nf," and rs=",rs,sep=""))
		dev.off()	

	}

}
printGraphAllEnv<-function(minus=F){
	
	for( nf in unique(kind$nf)){
		for( rs in unique(kind$rs)){
			mytitle=""
			if(minus)
				mytitle=paste("Diff beetwen the two pop\nfor nf=",nf," and rs=",rs,sep="")
			else
				mytitle=paste("Alive agent at t=2000\nfor nf=",nf," and rs=",rs,sep="")
			png(paste("alive_agent_wrNo0to200-NF",nf,"-RS",rs,"-Diff",minus,".png",sep=""))
			barplotAcacia(kind[kind$nf == nf& kind$rs == rs,], self[self$nf == nf& self$rs == rs,],"no","n_alive",xlab="Number of Obstacles",ylab="Agents Alive",minus=minus,main=mytitle)
			dev.off()	

		}
	}

}

#################################################
#Return a normalized matrix
createHeatMat<-function(x,y,data){
	library(plyr)
 	res=daply(.variables=c(x,y),.data=data,.fun=function(x)length(x[,1]))
	res[is.na(res)]<-0
	res=res/apply(res,1,sum)
	return(res)	
}

plotRealist <- function(dataz){
		res=dataz;
		res[,"n_realist"]=res[,"n_realist"]-res[,"n_realist"]%%3
		res=createHeatMat("Rep","n_realist",as.data.frame(res))
		     res.m=melt(res)
		     ggplot(res.m,aes(Rep,n_realist)) +
		     geom_tile(aes(fill = value)) +
		     scale_fill_gradient(low = "white",high = "orange2") +
		     scale_y_continuous("\nNumber of realist at t=3000",limits=c(0,100)) +
		     scale_x_continuous("\n% of selfish at t=0") +
		     opts(
			  panel.grid.major = theme_blank(),
			  panel.grid.minor = theme_blank(),
			  panel.background = theme_rect(),
			  axis.text.x=theme_text(col="black"),
			  axis.text.y=theme_text(col="black"),
			  title=paste("Number of Realist into the population \n after 3000 steps\n",sep="")
			  )
}

ggplotRatioHeatMap<-function(dataz,...){
	library(ggplot2)
	library(grid)
	res=dataz
	res[,"n_alive"]=(res[,"n_kind"]+1)/(res[,"n_selfish"]+1)
	res[,"n_alive"]=(res[,"n_kind"]+1)/(res[,"n_selfish"]+1)
	res[,"n_alive"]=round(log10(res[,"n_alive"]),1);
	res[,"n_alive"]=10^res[,"n_alive"]
	res=createHeatMat("Rep","n_alive",as.data.frame(res))
	res.m=melt(res)
	ggplot(res.m,aes(Rep,n_alive)) + 
	geom_tile(aes(fill = value)) +
	scale_fill_gradient(low = "white",high = "violetred4") +
	scale_y_log10(
		      expression(
				 bgroup("(",frac(kind+1,selfish+1),")")["t=2000"]
				 ,"\n")
		      ,breaks=c(0.01,0.1,0,10,100)
		      ,limits=c(.01,110)
		      ,labels = format(c(0.01,0.1,0,10,100),scientific=F)
		      ) +
	scale_x_continuous("\n% of selfish at t=0") +
	opts(
	     panel.grid.major = theme_blank(),
	     panel.grid.minor = theme_blank(),
	     panel.background = theme_rect(),
	     axis.text.x=theme_text(col="black"),
	     axis.text.y=theme_text(col="black"),
	     plot.margin = unit(c(0,0,0,0), "lines"),
	     title=paste("Composition of the population \n after 2000 steps\n",sep="")
	     )

}

