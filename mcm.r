data = read.csv("data.txt",header = T, sep = "\t", stringsAsFactors = F)
variables = read.csv("variablename.txt",header = T, sep = "\t", stringsAsFactors = F)
states = c("AZ","CA","NM","TX")
newestdata = data[which(data[,"Year"] == "2009"),]
azdata = data[which(data[,"StateCode"] == "AZ"),]
cadata = data[which(data[,"StateCode"] == "CA"),]
nmdata = data[which(data[,"StateCode"] == "NM"),]
txdata = data[which(data[,"StateCode"] == "TX"),]

num = 0
sapply(1:nrow(variables),function(x){
		vn = variables[x,1]
		vdata = azdata[which(azdata[,1]== vn),]
		if(nrow(vdata) != 0){
			pdf(paste(vn,"az.pdf", sep = ""))
			x = as.numeric(vdata[,"Year"])
			plot(x,as.numeric(vdata[,"Data"]), type = "l",main = variables[vn,"Description"])
			dev.off()
			num <<- num + 1
			print(num)
		}	
})
sapply(1:nrow(variables),function(x){
		vn = variables[x,1]
		vdata = cadata[which(cadata[,1]== vn),]
		if(nrow(vdata) != 0){
			pdf(paste(vn,"ca.pdf", sep = ""))
			x = as.numeric(vdata[,"Year"])
			plot(x,as.numeric(vdata[,"Data"]), type = "l",main = variables[vn,"Description"])
			dev.off()
			num <<- num + 1
			print(num)
		}	
})
sapply(1:nrow(variables),function(x){
		vn = variables[x,1]
		vdata = nmdata[which(nmdata[,1]== vn),]
		if(nrow(vdata) != 0){
			pdf(paste(vn,"nm.pdf", sep = ""))
			x = as.numeric(vdata[,"Year"])
			plot(x,as.numeric(vdata[,"Data"]), type = "l",main = variables[vn,"Description"])
			dev.off()
			num <<- num + 1
			print(num)
		}	
})
sapply(1:nrow(variables),function(x){
		vn = variables[x,1]
		vdata = txdata[which(txdata[,1]== vn),]
		if(nrow(vdata) != 0){
			pdf(paste(vn,"tx.pdf", sep = ""))
			x = as.numeric(vdata[,"Year"])
			plot(x,as.numeric(vdata[,"Data"]), type = "l",main = variables[vn,"Description"])
			dev.off()
			num <<- num + 1
			print(num)
		}	
})

###############################
#endusedata = azdata[which(rownames(azdata)=="TETXB"),]
secdata = azdata[c(which(rownames(azdata)=="TERCB"),which(rownames(azdata)=="TECCB"),which(rownames(azdata)=="TEICB"),which(rownames(azdata)=="TEACB")),]
tmp = sapply(1970:2009,function(x){
	c = as.numeric(secdata[which(secdata[,"Year"]==as.character(x)),"Data"])
	x/sum(x)	
})
a = apply(tmp,2, function(x){
	y = as.numeric(x)
	x = y/sum(y)
})
apply(a,1,function(x){
	1-t.test(x-mean(x))$p.value
})
#####sector separated

#####calculate the energy profile
# the ratio: RETCB + WWTCB + EMTCB + NUETB/ TETCB
# TETCB: total energy consumption
# arizona
azrenew = azdata[which(azdata[,"MSN"]=="RETCB"),]
aznuclear = azdata[which(azdata[,"MSN"]=="NUETB"),]
aztotal = azdata[which(azdata[,"MSN"]=="TETCB"),]
azratio = sapply(1960:2009,function(x){
	tmp = azrenew[which(azrenew[,"Year"]==x),"Data"] + aznuclear[which(aznuclear[,"Year"]==x),"Data"]
	tmp/aztotal[which(aztotal[,"Year"]==x),"Data"]
})
names(azratio) = 1960:2009
library(gcookbook)
library(ggplot2)
plotdata = data.frame(Year = 1960:2009, Ratio = azratio)
pdf("azratio.pdf")
plot = ggplot(data = plotdata, aes(x=Year, y=Ratio)) + 
geom_line(color = "blue") +
labs(y = "Renewable Energy Ratio", title = "Ratio for Arizona")+
stat_smooth(method="lm")
print(plot)
dev.off()
#ca
carenew = cadata[which(cadata[,"MSN"]=="RETCB"),]
canuclear = azdata[which(cadata[,"MSN"]=="NUETB"),]
catotal = cadata[which(cadata[,"MSN"]=="TETCB"),]
caratio = sapply(1960:2009,function(x){
	tmp = carenew[which(carenew[,"Year"]==x),"Data"] + canuclear[which(canuclear[,"Year"]==x),"Data"]
	tmp/catotal[which(catotal[,"Year"]==x),"Data"]
})
names(caratio) = 1960:2009
plotdata = data.frame(Year = 1960:2009, Ratio = caratio)
pdf("caratio.pdf")
plot = ggplot(data = plotdata, aes(x=Year, y=Ratio)) + 
geom_line(color = "blue") +
labs(y = "Renewable Energy Ratio", title = "Ratio for California") +
stat_smooth(method="lm")
print(plot)
dev.off()
#nm
nmrenew = nmdata[which(nmdata[,"MSN"]=="RETCB"),]
nmnuclear = nmdata[which(nmdata[,"MSN"]=="NUETB"),]
nmtotal = nmdata[which(nmdata[,"MSN"]=="TETCB"),]
nmratio = sapply(1960:2009,function(x){
	tmp = nmrenew[which(nmrenew[,"Year"]==x),"Data"] + nmnuclear[which(nmnuclear[,"Year"]==x),"Data"]
	tmp/nmtotal[which(nmtotal[,"Year"]==x),"Data"]
})
names(nmratio) = 1960:2009
plotdata = data.frame(Year = 1960:2009, Ratio = nmratio)
pdf("nmratio.pdf")
plot = ggplot(data = plotdata, aes(x=Year, y=Ratio)) + 
geom_line(color = "blue") +
labs(y = "Renewable Energy Ratio", title = "Ratio for New Mexico")+
stat_smooth(method="lm")
print(plot)
dev.off()
#tx
txrenew = txdata[which(txdata[,"MSN"]=="RETCB"),]
txnuclear = nmdata[which(txdata[,"MSN"]=="NUETB"),]
txtotal = txdata[which(txdata[,"MSN"]=="TETCB"),]
txratio = sapply(1960:2009,function(x){
	tmp = txrenew[which(txrenew[,"Year"]==x),"Data"] + txnuclear[which(txnuclear[,"Year"]==x),"Data"]
	tmp/txtotal[which(txtotal[,"Year"]==x),"Data"]
})
names(txratio) = 1960:2009
plotdata = data.frame(Year = 1960:2009, Ratio = txratio)
pdf("txratio.pdf")
plot = ggplot(data = plotdata, aes(x=Year, y=Ratio)) + 
geom_line(color = "blue") +
labs(y = "Renewable Energy Ratio", title = "Ratio for Texas")+
stat_smooth(method="lm")
print(plot)
dev.off()
#all ratio
plotdata = 
pdf("allratio.pdf")
plot = ggplot(data = )


###########Grey Model################
grey_model = function(ts){  #tm = time series data
	tsp = ts[ts != 0] #time series positive
	if(length(tsp) == 0){
		result = rep(0,times = 41)
		return(list(result = result, C = -1, q = -1, P = -1))
	}
	if(length(tsp)==1){
		result = rep(tsp, times = 41)
		return(list(result = result, C = -1, q = -1, P = -1))
	}
	if(length(tsp) < 10){
		data = cbind(1:length(tsp), tsp)
		colnames(data) = c("x","y")
		data = data.frame(data)
		m = lm(y~x,data = data)
		result = sapply((length(tsp) + 1):(length(tsp) + 41),function(x){
			sum(c(1,x)*coef(m))
		})
		return(list(result = result, C = -1, q = -1, P = -1))
	}else{
		cs = cumsum(tsp)
		tmp = -0.5*(cs[1:(length(cs)-1)]+cs[2:length(cs)])
		B = cbind(tmp,rep(1, times = length(tmp)))
		Y = as.matrix(tsp[2:length(tsp)])
		coefgrey = solve(t(B) %*% B) %*% t(B) %*% Y     #least square estimate sequence
		a = coefgrey[1]
		u = coefgrey[2]
		cumresult = sapply(0:(length(cs) + 41),function(t){
			(tsp[1] - u/a) * exp(-a*t) + u/a
		})
		result1 = c(cumresult[1],diff(cumresult))[(length(cs) + 1):(length(cs) + 41)]
		result2 = c(cumresult[1],diff(cumresult))[1:(length(cs) + 41)]
		residual = tsp - result2[1:length(tsp)]
		qbar = mean(residual/tsp)  #relative error
		rebar = mean(residual)
		resd = sd(residual)
		tspsd = sd(tsp)
		C = resd/tspsd
		P = sum(abs(residual - rebar) < 0.6745 * tspsd)/length(residual)
		return(list(result = result1, allresult = result2, C = C, q = qbar, P = P))
	
	}
	
}

##########Multiple Linear Analysis
#multiple Linear Analysis on the whole and each sector
#independent variables: coal price, natural gas price, all petroleum price, population, GDP, production of renewable energy 
#notice: the price should be adjusted according to inflation rate
#dependent variable: the ratio
cpi = read.csv("CPI.txt", header = F)
rownames(cpi) = 1960:2009
baseyrindex = cpi["2005",]
alldata = list(AZ = azdata, CA = cadata, NM = nmdata, TX = txdata)
ratiodata = list(AZ = azratio, CA = caratio, NM = nmratio, TX = txratio)
adjprice = function(p){
	(p/baseyrindex)*cpi[21:50,]
}
tmp = sapply(states,function(sn){          #sn = state names
	modeldata = sapply(1980:2009, function(x){
		alldata[[sn]][alldata[[sn]][,"Year"]==x,"Data"]
	})
	modeldata = t(modeldata)
	rownames(modeldata) = 1980:2009
	colnames(modeldata) = levels(as.factor(alldata[[sn]][,"MSN"]))
	modeldata = data.frame(modeldata)
	prices = c("CLTCD","NGTCD","PATCD")
	tmp = sapply(prices,function(x){
		modeldata[[x]] <<- adjprice(modeldata[[x]])
	})
	modeldata = data.frame(modeldata,ratio = ratiodata[[sn]][21:50])
	lm.sol = lm(ratio ~ CLTCD + NGTCD + PATCD + CLPRB + NGMPB + PAPRB + EMFDB + GETCB + HYTCB + SOTCB + WYTCB + WWTCB + GDPRX + TPOPP + NUETB, data = modeldata)
	print(summary(lm.sol))
	lm.step = step(lm.sol)
})
#REPRB = EMFDB + GETCB + HYTCB + SOTCB + WYTCB + WWTCB
#az ratio ~ CLTCD + PAPRB + REPRB + TPOPP + NUETBB
#ca ratio ~ REPRB + GDPRX + TPOPP
#nm ratio ~ NGTCD+ PAPRB + REPRB
#tx ratio ~ NGTCD + PATCD + NGMPB + REPRB + NUETB
##updated model
cut = function(x){
	if(x >= 1){
		return(1)
	}else if(x<=0){
		return(0)
	}else{
		return(x)
	}
}
#AZ
sn = "AZ"
modeldata = sapply(1980:2009, function(x){
	alldata[[sn]][alldata[[sn]][,"Year"]==x,"Data"]
})
modeldata = t(modeldata)
rownames(modeldata) = 1980:2009
colnames(modeldata) = levels(as.factor(alldata[[sn]][,"MSN"]))
modeldata = data.frame(modeldata)
prices = c("CLTCD","NGTCD","PATCD")
tmp = sapply(prices,function(x){
	modeldata[[x]] <<- adjprice(modeldata[[x]])
})
modeldata = data.frame(modeldata,ratio = ratiodata[[sn]][21:50])
az.sol = lm(ratio ~ PATCD + PAPRB + EMFDB + HYTCB+ WYTCB + TPOPP + NUETB, data = modeldata)
summary(az.sol)
co = coef(az.sol) #coefficients
idva = c("PATCD","PAPRB","EMFDB","HYTCB","WYTCB","TPOPP","NUETB")
predictdata = modeldata[,idva]
az.greycp = matrix(-1,nrow =length(idva),ncol = 2)
colnames(az.greycp) = c("c","p")
pr = sapply(1:ncol(predictdata), function(x){ #pr predicted result
	gm = grey_model(predictdata[,x])
	az.greycp[x,1] <<- gm[["C"]]
	az.greycp[x,2] <<- gm[["P"]]
	gm[["result"]]
})
rownames(pr) = 2010:2050
azpr = apply(pr,1,function(x){
	cut(sum(c(1,x)*co)) 
})
#CA
sn = "CA"
modeldata = sapply(1980:2009, function(x){
	alldata[[sn]][alldata[[sn]][,"Year"]==x,"Data"]
})
modeldata = t(modeldata)
rownames(modeldata) = 1980:2009
colnames(modeldata) = levels(as.factor(alldata[[sn]][,"MSN"]))
modeldata = data.frame(modeldata)
prices = c("CLTCD","NGTCD","PATCD")
tmp = sapply(prices,function(x){
	modeldata[[x]] <<- adjprice(modeldata[[x]])
})
modeldata = data.frame(modeldata,ratio = ratiodata[[sn]][21:50])
ca.sol = lm(ratio ~ CLTCD + HYTCB + GDPRX + TPOPP, data = modeldata)
summary(ca.sol)
co = coef(ca.sol) #coefficients
idva = c("CLTCD","HYTCB","GDPRX","TPOPP")
predictdata = modeldata[,idva]
ca.greycp = matrix(-1,nrow =length(idva),ncol = 2)
colnames(ca.greycp) = c("c","p")
pr = sapply(1:ncol(predictdata), function(x){ #pr predicted result
	gm = grey_model(predictdata[,x])
	ca.greycp[x,1] <<- gm[["C"]]
	ca.greycp[x,2] <<- gm[["P"]]
	gm[["result"]]
})
rownames(pr) = 2010:2050
capr = apply(pr,1,function(x){
	cut(sum(c(1,x)*co)) 
})
#NM
sn = "NM"
modeldata = sapply(1980:2009, function(x){
	alldata[[sn]][alldata[[sn]][,"Year"]==x,"Data"]
})
modeldata = t(modeldata)
rownames(modeldata) = 1980:2009
colnames(modeldata) = levels(as.factor(alldata[[sn]][,"MSN"]))
modeldata = data.frame(modeldata)
prices = c("CLTCD","NGTCD","PATCD")
tmp = sapply(prices,function(x){
	modeldata[[x]] <<- adjprice(modeldata[[x]])
})
modeldata = data.frame(modeldata,ratio = ratiodata[[sn]][21:50])
nm.sol = lm(ratio ~ CLTCD + PATCD + NGMPB + HYTCB + WYTCB + WWTCB, data = modeldata)
summary(nm.sol)
co = coef(nm.sol) #coefficients
idva = c("CLTCD","PATCD","NGMPB", "HYTCB", "WYTCB", "WWTCB")
predictdata = modeldata[,idva]
nm.greycp = matrix(-1,nrow =length(idva),ncol = 2)
colnames(nm.greycp) = c("c","p")
pr = sapply(1:ncol(predictdata), function(x){ #pr predicted result
	gm = grey_model(predictdata[,x])
	nm.greycp[x,1] <<- gm[["C"]]
	nm.greycp[x,2] <<- gm[["P"]]
	gm[["result"]]
})
rownames(pr) = 2010:2050
nmpr = apply(pr,1,function(x){
	cut(sum(c(1,x)*co)) 
})
#TX
sn = "TX"
modeldata = sapply(1980:2009, function(x){
	alldata[[sn]][alldata[[sn]][,"Year"]==x,"Data"]
})
modeldata = t(modeldata)
rownames(modeldata) = 1980:2009
colnames(modeldata) = levels(as.factor(alldata[[sn]][,"MSN"]))
modeldata = data.frame(modeldata)
prices = c("CLTCD","NGTCD","PATCD")
tmp = sapply(prices,function(x){
	modeldata[[x]] <<- adjprice(modeldata[[x]])
})
modeldata = data.frame(modeldata,ratio = ratiodata[[sn]][21:50])
tx.sol = lm(ratio ~ CLTCD + HYTCB + SOTCB + WYTCB + WWTCB + GDPRX, data = modeldata)
summary(tx.sol)
co = coef(tx.sol) #coefficients
idva = c("CLTCD","HYTCB","SOTCB", "WYTCB", "WWTCB","GDPRX")
predictdata = modeldata[,idva]
tx.greycp = matrix(-1,nrow =length(idva),ncol = 2)
colnames(tx.greycp) = c("c","p")
pr = sapply(1:ncol(predictdata), function(x){ #pr predicted result
	if(x != 4){
		gm = grey_model(predictdata[,x])
		tx.greycp[x,1] <<- gm[["C"]]
		tx.greycp[x,2] <<- gm[["P"]]
		gm[["result"]]
	}else{
		ts = predictdata[,x]
		tsp = ts[ts != 0]
		data = cbind(1:length(tsp), tsp)
		colnames(data) = c("x","y")
		data = data.frame(data)
		m = lm(y~log(x),data = data)
		result = sapply((length(tsp) + 1):(length(tsp) + 41),function(x){
			sum(c(1,log(x))*coef(m))
		})
	}
})
rownames(pr) = 2010:2050
txpr = apply(pr,1,function(x){
	cut(sum(c(1,x)*co)) 
})


#################################################
##qq plot
pdf("azqqplot.pdf")
plot(az.sol,2,pch = 16)
plot(az.sol,1,pch = 16)
dev.off()

pdf("caqqplot.pdf")
plot(ca.sol,2,pch = 16)
plot(ca.sol,1,pch = 16)
dev.off()

pdf("nmqqplot.pdf")
plot(nm.sol,2,pch = 16)
plot(nm.sol,1,pch = 16)
dev.off()

pdf("txqqplot.pdf")
plot(tx.sol,2,pch = 16)
plot(tx.sol,1,pch = 16)
dev.off()

az.greycp
ca.greycp
nm.greycp
tx.greycp

####prediction plot
plotdata = data.frame(linegroup = c(rep(c("predicted"),each = 71*4),rep("original",each = 30*4)), state = c(rep(states,each = 71),rep(states,each = 30)),group = c(rep(states,each = 71),rep(c("AZ predicted", "CA predicted", "NM predicted", "TX predicted"),each = 30)),year = c(rep(1980:2050,times = 4),rep(1980:2009,times = 4)), pr = c(c(az.sol$fitted.values,azpr),c(ca.sol$fitted.values,capr), c(nm.sol$fitted.values,nmpr), c(tx.sol$fitted.values,txpr),azratio[21:50],caratio[21:50],nmratio[21:50],txratio[21:50]))
pdf("prediction.pdf")
plot = ggplot(data = plotdata, aes(x = year, y = pr, group = group))+
geom_line(aes(linetype = linegroup, color = state)) + 
labs(title = "Model Result", x = "Year", y = "Renewable Energy Ratio")
print(plot)
dev.off()








