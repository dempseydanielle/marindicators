herringAggregate <- function(path1=p) {
	#her <- sqlQuery(channel,paste('select year,area,spec,abundance*1000 abundance,biomass*1000 biomass,wt from indiseas_4X_herring'))
	  her = read.csv(file.path(path1,'extra info','4xherringbiomassDec2015.csv'))
	  her$ABUNDANCE = her$ABUNDANCE*1000
	  her$BIOMASS = her$BIOMASS*1000
	  
  dat <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, spec,totno*1.75/i.dist Abundance,totwgt*1.75/i.dist biomass from 
					groundfish.gsinf i, groundfish.gscat c where i.mission=c.mission and i.setno=c.setno and to_char(sdate,'yyyy') between 1970 and 2015 and to_char(sdate,'mm') in ('06','07','08') and
					strat between '470' and '495' and type=1 and spec=60",sep=""))
	dat.full <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, 60 spec, 0 Abundance,0 biomass from 
					groundfish.gsinf i where to_char(sdate,'yyyy') between 1970 and 2015 and to_char(sdate,'mm') in ('06','07','08') and
					strat between '470' and '495' and type=1 ;",sep=""))
	aa <- merge(dat,dat.full,all=T)
	dat <- aa[!duplicated(aa[,1:6],fromLast=T),]
	wts <- sqlQuery(channel,paste("select strat,area/((41./6080.2)*1.75) tunits from groundfish.gsstratum where strat between '470' and '495';"))
	
	#mean within strat
	dat.agg <- as.data.frame(aggregate(dat[,c('ABUNDANCE','BIOMASS')],by=dat[c('YEAR','STRAT')],FUN='mean'))
	dat.agg1 <- merge(dat.agg,wts,by='STRAT')
	
	#total abundance
	dat.agg1$AEST <- dat.agg1[,'ABUNDANCE']*dat.agg1[,'TUNITS']
	dat.agg1$BEST <- dat.agg1[,'BIOMASS']*dat.agg1[,'TUNITS']
	
	#overall abundance
	dat.agg2 <- as.data.frame(aggregate(dat.agg1[,c('AEST','BEST')],by=(dat.agg1['YEAR']),FUN='sum'))
	names(dat.agg2)[2] <-'AGGAEST'
	names(dat.agg2)[3] <-'AGGBEST'
	
	dat.agg3 <- merge(dat.agg1,dat.agg2,by='YEAR')
	
	#proportion of stratified abundance within each strata
	dat.agg3$PROP.A <- dat.agg3$AEST/dat.agg3$AGGAEST
	dat.agg3$PROP.B <- dat.agg3$BEST/dat.agg3$AGGBEST

	#proportioning out the overall abunance into strata and dividing by tunits to get mean per tow
	
	dat4 <- merge(dat.agg3,her,by='YEAR')
	dat4$A <- dat4$PROP.A * dat4$ABUNDANCE.y/dat4$TUNITS
	dat4$B <-dat4$PROP.B * dat4$BIOMASS.y/dat4$TUNITS
	
	dat4 <- dat4[,c('YEAR','STRAT','A','B')]
	
	#proportion of mean at each set to use to redistribute the total biomass estimate
	dat5 <- merge(dat,dat.agg,by=c('YEAR','STRAT'))
	dat6 <- aggregate(dat5$YEAR,by=dat5[c('YEAR','STRAT')],FUN=length)
	dat7 <- merge(dat5,dat6,by=c('YEAR','STRAT'))
	
	dat7$Abyset <- with(dat7,(ABUNDANCE.x)/(ABUNDANCE.y))
	dat7$Bbyset <- with(dat7,(BIOMASS.x)/(BIOMASS.y))
	
	dat8 <- merge(dat7,dat4,by=c('YEAR','STRAT'))
	dat8$TOTNO <- with(dat8,A*Abyset)
	#dat8$TOTWGT <- with(dat8,B*Bbyset)
	# her.dat <- dat8[,c(3,4,5,6,1,2,7,17,18)]
	#  names(her.dat)[8:9] <- c('TOTNO','TOTWGT')
	#  hh <- her.dat
 	#hh[is.na(hh)] <-0
 	
	
	#changed weights to match mean annual weight
	dat8 <- merge(dat8,her,by='YEAR')	
	dat8$TOTWGT <- with(dat8,TOTNO*WT)
	
	 her.dat <- dat8[,c(3,4,5,6,1,2,7,17,23)]
		  hh <- her.dat
		hh <- hh[,-which(names(hh) %in% c('YDDMMSS', 'XDDMMSS'))]

		   	hh[is.na(hh)] <-0
 	 

 	names(hh) <-c('MISSION','SETNO','YEAR','STRAT','SPEC','ABUNDANCE','BIOMASS')
 	return(hh)
 	
	}