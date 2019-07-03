#for the herring at length information
#fixed September 06, 2013 02:39:55 PM  There were problems with allocating missing lengths in previous script now called VPAherringLengthOld
	herring.at.Length <- function(path1=p) {
		dat <- sqlQuery(channel,paste("select i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year,flen, c.spec,clen*nvl(decode(totwgt,0,1,totwgt),1)/nvl(decode(sampwgt,0,1,sampwgt),1)*1.75/dist Abundance from 
						groundfish.gsinf i, groundfish.gscat c, groundfish.gsdet d where i.mission=c.mission and i.setno=c.setno and i.mission=d.mission and i.setno=d.setno and c.spec=d.spec
						and to_char(sdate,'yyyy') between 1970 and 2015 and to_char(sdate,'mm') in ('06','07','08') and  strat between '470' and '495' and type=1 and c.spec=60",sep=""))
		#not using the herring at length as age length keys cover more sizes than are observed in the survey
		
		
		dat.full <- sqlQuery(channel,paste("select distinct i.mission,i.setno,i.strat,slat YDDMMSS, slong*-1 XDDMMSS,to_char(sdate,'yyyy') year, 60 spec, 0 Abundance from 
						groundfish.gsinf i where to_char(sdate,'yyyy') between 1970 and 2015 and to_char(sdate,'mm') in ('06','07','08') and
				strat between '470' and '495' and type=1 ;",sep=""))
		print('need to change the years to match full data series in herring.at.Length.r')
		dat.full$ID <- 1:nrow(dat.full)
		
		#observed length and year
		FL <- unique(dat[,c('FLEN','YEAR')])
		FL <- FL[order(FL$YEAR,FL$FLEN),]
		y <- unique(FL$YEAR)
		
					#Full dataset
		ou <- NULL
		for(i in 1:length(y)) {
				d <- dat.full[dat.full$YEAR==y[i],]
				e <- FL[FL$YEAR==y[i],'FLEN']
				l <- length(e)
				d.f<- d[rep(seq_len(nrow(d)), each=l),]
				d.f$FLEN <- e
				ou <- rbind(ou,d.f)
		}	
		
		
			aa <- merge(dat,ou,all=T)
	dat <- aggregate(ABUNDANCE~YEAR+STRAT+FLEN+MISSION+SETNO,data=dat,FUN=sum)
	#strata weights
	wts <- sqlQuery(channel,paste("select strat,area, area/((41./6080.2)*1.75) tunits from groundfish.gsstratum where strat between '470' and '495';"))
	
	#mean within strat
	
	dat.agg <- aggregate(ABUNDANCE~YEAR+STRAT+FLEN,data=dat,FUN='mean')
	dat.agg1 <- merge(dat.agg,wts,by='STRAT')
	
	#total abundance
	

	dat.agg1$AEST <- dat.agg1$ABUNDANCE*dat.agg1$TUNITS
	
	
	#overall abundance
	dat.agg2 <- aggregate(AEST~YEAR+FLEN,data=dat.agg1,FUN='sum')
	
		
	dat.agg3 <- merge(dat.agg1,dat.agg2,by=c('YEAR','FLEN'))
	
	#proportion of stratified abundance within each strata
	dat.agg3$PROP.A <- dat.agg3$AEST.x/dat.agg3$AEST.y
	

		
		#partioning herring total abundance and biomass
		#her <- sqlQuery(channel,paste('select * from indiseas_4X_herring'))
		#her.l <- sqlQuery(channel,paste('select * from indiseas_4X_herring_at_len'))
	
		her = read.csv(file.path(path1,'extra info','4xherringbiomassDec2015.csv'))
	  her.l = read.csv(file.path(path1,'extra info','4xherringatlengthDec2015.csv'))
		print('update the herring at length table in extra info for years past 2014...amc dec 2015')
			her$BIOMASS<-her$BIOMASS*1000; her$ABUNDANCE<-her$ABUNDANCE*1000
			her.l$BIOMASS<-her.l$BIOMASS*1000;	her.l$ABUNDANCE<-her.l$ABUNDANCE*1000
		
		be <- merge(her.l,FL,by=c('YEAR','FLEN'),all.y=T)
		be <- merge(be,aggregate(cbind(ABUNDANCE,BIOMASS)~YEAR,data=be,FUN=sum),by='YEAR')


		be$pBiom <- be$BIOMASS.x/be$BIOMASS.y
		be$pAbund <- be$ABUNDANCE.x/be$ABUNDANCE.y
		
		be <- merge(be[,c('YEAR','FLEN','pBiom','pAbund')],her,by=c('YEAR'))
		be$ABUNDANCE <- be$ABUNDANCE*be$pAbund
		be$BIOMASS <- be$BIOMASS*be$pBiom
		#her is now the total biomass and abudance proportions based on the size classes observed in the survey and proportions based on generated age length key September 06, 2013 AMC AB
		her <- be[,c('YEAR','FLEN','ABUNDANCE','BIOMASS')]
		her$WT <- her$BIOMASS/her$ABUNDANCE
		
			

	#proportioning out the overall abunance into strata and dividing by tunits to get mean per tow
	
	dat4 <- merge(dat.agg3,her,by=c('YEAR','FLEN'))
	dat4$A <- dat4$PROP.A* dat4$ABUNDANCE.y/dat4$TUNITS
	dat4 <- dat4[,c('YEAR','STRAT','FLEN','A')] #mean abunance in each strata by year and flen
	
	
	
	#proportion of mean at each set to use to redistribute the total biomass estimate
	dat.agg9 <- aggregate(ABUNDANCE~YEAR+STRAT+FLEN,data=dat,FUN='sum')
	dat5 <- merge(dat,dat.agg9,by=c('YEAR','STRAT','FLEN'))
	dat6 <- aggregate(SETNO~YEAR+STRAT+FLEN,data=dat5,FUN=length)
	names(dat6)[ncol(dat6)] <-'NSETS'
	dat7 <- merge(dat5,dat6,by=c('YEAR','STRAT','FLEN'))
	dat7$Abyset <- with(dat7,(ABUNDANCE.x)/(ABUNDANCE.y))
	
	
	dat8 <- merge(dat7,dat4,by=c('YEAR','STRAT','FLEN'))
	dat8$TOTNO <- with(dat8,A*NSETS*Abyset)
	dat8 <- merge(dat8,her,by=c('YEAR','FLEN'))	
	dat8$TOTWGT <- with(dat8,TOTNO*WT)
	#
	 her.dat <- dat8[,c(1:5,11,15)]
		  hh <- her.dat
 	hh[is.na(hh)] <-0
	 names(hh)[6:7] <- c('ABUNDANCE','BIOMASS')
 hh$QBIOMASS <- hh$BIOMASS <- hh$BIOMASS #in KG
 hh$QABUNDANCE<- hh$ABUNDANCE
 hh$SPECIES <- 60
return(hh)
 }
