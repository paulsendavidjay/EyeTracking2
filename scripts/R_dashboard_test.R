library(RMySQL)
library(ggplot2)
library(reshape)
library(grid)
library(gridExtra)



####################################################################################
############			GET DATA FROM DATABASE
# ESTABLISH DATABASE CONNECTION
con <- dbConnect(MySQL(),
         user="root", password="",
         dbname="test", host="127.0.0.1")

query <- "
SELECT subjectID, AgeGroup, cv1, gamble, RT, riskData.condition
FROM riskData
WHERE gamble <> 99;
"
riskData <- dbGetQuery(con, query)

# DISCONNECT FROM DB
dbDisconnect(con)




####################################################################################
############			PUT DATA INTO APPROPRIATE FORMAT

# DATA MANIPULATION AND SUMMARIES
riskData$cv <- round(riskData$cv1, 3)
riskData$cv1 <- NULL
riskData$condition <- factor(riskData$condition, levels=c(0,1), labels=c("Loss","Gain"))

# RISK TAKING
gamble_summary_individual <- ddply(riskData, .(subjectID, AgeGroup, condition, cv),
	.fun = function(xx) { c(gamble = mean(xx[,"gamble"]))})

gamble_summary_group <- ddply(gamble_summary_individual, .(AgeGroup, condition, cv),
	.fun = function(xx) { 
		c(gamble = mean(xx[,"gamble"]), 
		se = sd(xx[,"gamble"],na.rm=T) / sqrt(length(!is.na(xx[,"gamble"]))) )
		})

# RESPONSE TIME
RT_summary_individual <- ddply(riskData, .(subjectID, AgeGroup, condition, cv),
	.fun = function(xx) { c(RT = mean(xx[,"RT"]))})

RT_summary_group <- ddply(RT_summary_individual, .(AgeGroup, condition, cv),
	.fun = function(xx) { 
		c(RT = mean(xx[,"RT"]), 
		se = sd(xx[,"RT"],na.rm=T) / sqrt(length(!is.na(xx[,"RT"]))) )
		})

####################################################################################
############			GENERATE PLOTS

plot_riskXageGroup.gain <- ggplot(subset(gamble_summary_group, condition=="Gain"), aes(x=factor(cv), y=gamble, group=AgeGroup)) +
	geom_line(aes(color=AgeGroup)) + 
	geom_point() +
	geom_errorbar(aes(ymin=gamble-se, ymax=gamble+se), colour="black", width=.1) +
	ylim(0,1) +
	labs(title="Gain Domain Gambles", x="Risk Level",y="Prop. Gamble",colour="Age Group")

plot_riskXageGroup.loss <- ggplot(subset(gamble_summary_group, condition=="Loss"), aes(x=factor(cv), y=gamble, group=AgeGroup)) +
	geom_line(aes(color=AgeGroup)) + 
	geom_point() +
	geom_errorbar(aes(ymin=gamble-se, ymax=gamble+se), colour="black", width=.1) +
	ylim(0,1) +
	labs(title="Loss Domain Gambles", x="Risk Level",y="Prop. Gamble",colour="Age Group") 

plot_riskXageGroup.gainRT <- ggplot(subset(RT_summary_group, condition=="Gain"), aes(x=factor(cv), y=RT, group=AgeGroup)) +
	geom_line(aes(color=AgeGroup)) + 
	geom_point() +
	ylim(2,10) +
	geom_errorbar(aes(ymin=RT-se, ymax=RT+se), colour="black", width=.1) +
	labs(title="Gain Domain RT", x="Risk Level",y="RT (sec)",colour="Age Group") 

plot_riskXageGroup.lossRT <- ggplot(subset(RT_summary_group, condition=="Loss"), aes(x=factor(cv), y=RT, group=AgeGroup)) +
	geom_line(aes(color=AgeGroup)) + 
	geom_point() +
	ylim(2,10) +
	geom_errorbar(aes(ymin=RT-se, ymax=RT+se), colour="black", width=.1) +
	labs(title="Loss Domain RT", x="Risk Level",y="RT (sec)",colour="Age Group") 


####################################################################################
############			SET ADDITIONAL PLOTTING PARAMETERS

# OBJECT CONTAINING GENERAL FORMATTING PARAMETERS
plot_format <- 	theme(panel.background=element_blank()) + 
	theme(axis.text.x = element_text(size=6),
		axis.text.y = element_text(size=6),
		axis.title.x = element_text(size=8),
		axis.title.y = element_text(size=8),
		legend.text = element_text(size=8),
		legend.title = element_text(size=8),
		legend.key.size = unit(3, "mm"), 
		strip.text.x = element_text(size=9),
#		plot.background=element_blank(), # for transparency
#		panel.background=element_blank(), # for transparency
		panel.grid.minor = element_line(color="gray70", linetype="solid"), 
	    panel.grid.major = element_line(color="gray90", linetype="dashed", size=0.25), 
		title = element_text(size=12)
) 


# g_legend<-function(a.gplot){
	# tmp <- ggplot_gtable(ggplot_build(a.gplot))
	# leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
	# legend <- tmp$grobs[[leg]]
	# return(legend)
# }

# legend <- g_legend(plot_riskXageGroup.gain)
# lwidth <- sum(legend$width)


####################################################################################
############			PUT PLOTS INTO PDF

# LAYOUT GRID FOR DASHBOARD
Layout = grid.layout(nrow = 3, ncol = 2,
	widths = unit(c(2, 2), rep("null", 2)), 
	heights = unit(c(0.75, 2, 2), rep("null",3))) # units are supposed to be 'null'

# VIEWPORT FUNCTION TO CUT DOWN ON TEXT
put_plot <- function(row, col) { viewport(layout.pos.row = row, layout.pos.col = col)}

pdf(file="/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze2/analysis/test_output.pdf", width = 7, height = 5,bg="transparent") 


#png(file="/Users/dpaulsen/Documents/Academics/Projects/EyeTrackingGaze2/analysis/test_output_trans.png",width=800,height=600,units="px",bg = "transparent")
	pushViewport(viewport(layout = Layout)) 

	print(plot_riskXageGroup.gain + plot_format, vp = put_plot(2,1)) 
	print(plot_riskXageGroup.loss + plot_format, vp = put_plot(2,2)) 
	print(plot_riskXageGroup.gainRT + plot_format, vp = put_plot(3,1)) 
	print(plot_riskXageGroup.lossRT + plot_format, vp = put_plot(3,2)) 

	grid.text("Risk Preference & RT: Age Group Comparisons", 
		x = unit(0.5, "npc"), 
		y = unit(0.95, "npc"), 
		just = "center", 
		gp = gpar(col = "black", fontsize = 20))

	grid.text(date(), 
		x = unit(0.5, "npc"), 
		y = unit(0.9, "npc"), 
		just = "center", 
		gp = gpar(col = "black", fontsize = 10))

dev.off() 




cast_example <- cast(gamble_summary_group, AgeGroup + condition ~ cv, value="gamble")
melt(cast_example, id.vars=c("AgeGroup","condition"), variable_name="cv", value.name="gamble")





