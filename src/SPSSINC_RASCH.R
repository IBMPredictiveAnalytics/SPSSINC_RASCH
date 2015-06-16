#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 1989, 2014
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# Author: Alex Reutter, IBM SPSS
#
# Version: 1.3.0
# history
# 2011 - original version
# 1-1-2013 bugfix ECM186139
# 4-4-2013 add ability to pin the discrimination parameter (alpha)

helptext="The SPSSINC RASCH command requires the R Integration Plug-in and the R ltm package.

SPSSINC RASCH 
  /VARIABLES ITEMS=variable list
  [/OPTIONS [MISSING={LISTWISE**  }]  [EXECUTE={TRUE**}] ] [ALPHA=number]
                   {ALLAVAILABLE}            {FALSE }

  [/PRINT [SUMMARY] [ITEMFIT]]
  [/PLOT [DESCRIPTIVES] [FACTORSCORES] [ICC] [IIC]]
  [/SAVE [PROGRAMFILE=filespec]
 		 [PERSONFITDATASET=datasetname]]

Split files and weight are not honored by this command.

SPSSINC RASCH /HELP prints this information and does nothing else.

Example:
SPSSINC RASCH 
  /VARIABLES ITEMS=item1 item2 item3 item4 item5.

Executes the rasch function from the R ltm package.
The variable list specifies the items.  It is assumed that
the values of these variables is 0,1.

/OPTIONS MISSING=LISTWISE causes listwise deletion of missing values.  ALLAVAILABLE
causes all available information to be used; i.e., cases with missing values
are still used.

EXECUTE=FALSE runs the command syntax without calling the R function. 
This is mainly useful in combination with SAVE PROGRAMFILE.

By default, the discrimination parameter, alpha, is estimated.  Use
ALPHA=number to constrain it to a specific value, typically 1.

/PRINT SUMMARY displays the Akaike Information Criterion (AIC) and Bayesian Information
Criterion (BIC) for the model.

ITEMFIT displays the item fit statistics and p-values for each item.

/PLOT DESCRIPTIVES displays a plot of the proportion of correct responses for each 
item versus the total correct.

FACTORSCORES displays a plot of kernel density estimates for the factor scores 
(people parameters).

ICC displays a plot of the item characteristic curves for the model.

IIC displays a plot of the iterm information curves for the model.

/SAVE PROGRAMFILE causes the R code that implements the procedure to be 
written to the specified file. Since the R function has features not 
exposed in this extension command, the generated program can be a useful 
starting point for additional specifications.

PERSONFITDATASET creates a new dataset that contains a case for each observed 
response pattern, variables that describe the response patterns (one for each
item), and variables containing the Levine and Rubin person-fit statistic, 
Drasgow person-fit statistic, P-value for Drasgow person-fit statistic,
Observed frequency, Expected frequency, Factor scores, Factor scores standard errors,
and Residuals.
"

run_rasch<-function(items, missing="listwise", print_summary=FALSE, print_itemfit=FALSE, plot_descriptives=FALSE,
		   plot_factorscores=FALSE, plot_icc=FALSE, plot_iic=FALSE, personfitdataset=NULL, alpha=NULL)
{
    domain<-"SPSSINC_RASCH"
	setuplocalization(domain)
    
    tryCatch(library(ltm), error=function(e){
        stop(gettextf("The R %s package is required but could not be loaded.","ltm",domain=domain),call.=FALSE)
        }
    )

    if (identical(missing,"listwise")) {missing<-na.exclude} else {missing<-NULL}
    
    dta<-spssdata.GetDataFromSPSS(items,missingValueToNA=TRUE)

	if (!is.null(alpha)) {
		caption = sprintf(gettext("Discrimination parameter constrained to %.4f", domain=domain), alpha)
	} else {
		caption	= ""
	}
	if (is.null(alpha)) {	
		res <- tryCatch(
            rasch(data=dta,na.action=missing),
            error=function(e) {return(c(gettext("ERROR:",domain=domain),e))}
		)
	} else {
		res <- tryCatch(
            rasch(data=dta,na.action=missing, constraint=cbind(ncol(dta) + 1, alpha)),
            error=function(e) {return(c(gettext("ERROR:",domain=domain),e))}
		)
	}
   
    if (!is.null(res$message)) {print(res$message)} else {

		miss<-ifelse(identical(missing,na.exclude),"na.exclude","NULL")
		
		# Start sending output to Viewer
        StartProcedure(gettext("Rasch Model",domain=domain), "SPSSINC RASCH")

		# Summary table
		if ( print_summary ) {
			information_criteria <- matrix( c(summary(res)$AIC,summary(res)$BIC), ncol=1 )
			dimnames(information_criteria)[[1]] <- c(gettext("Akaike Information Criterion (AIC)",domain=domain),gettext("Bayesian Information Criterion (BIC)",domain=domain))
			dimnames(information_criteria)[[2]] <- c(gettext("Value",domain=domain))
			spsspivottable.Display(information_criteria, 
				title=gettext("Summary",domain=domain),
				templateName="SPSSINCRaschSummary",
				isSplit=FALSE)
		}
		
		# Coefficients table
        coeff <- summary(res)$coefficients
		dimnames(coeff)[[1]] <- c(paste(gettext("Difficulty:",domain=domain),names(item.fit(res)$Tobs)), gettext("Discrimination",domain=domain))
		dimnames(coeff)[[2]] <- c(gettext("Value",domain=domain),gettext("Standard Error",domain=domain),gettext("Z",domain=domain))
		spsspivottable.Display(coeff, 
			title=gettext("Coefficients",domain=domain),
			templateName="SPSSINCRaschCoefficients",caption=caption,
			isSplit=FALSE)
		
			
		# Item fit
		if ( print_itemfit ) {
			item_fit <- matrix( c(item.fit(res)$Tobs,item.fit(res)$p.value), ncol=2, byrow=F )
			dimnames(item_fit)[[1]] <- c(names(item.fit(res)$Tobs))
			dimnames(item_fit)[[2]] <- c(gettext("Chi-square",domain=domain),gettext("Sig.",domain=domain))
			spsspivottable.Display(item_fit, 
				title=gettext("Item Fit Statistics",domain=domain),
				templateName="SPSSINCRaschItemFit",
				isSplit=FALSE)
		}
		
		# Descriptives plot
		if ( plot_descriptives ) {
		   if (length(items)<3)
		        warning(gettext("Failed to create Descriptives plot. Descriptives plot requires 3 or more items.",domain=domain),call.=FALSE)
		   else
			plot(descript(dta))
		}

		# Factor scores plot
		if ( plot_factorscores ) {
			plot(factor.scores(res))
		}

		# Item characteristic curves plot
		if ( plot_icc ) {
			plot(res, type="ICC")
		}

		# Descriptives plot
		if ( plot_iic ) {
			plot(res, type="IIC")
		}

		spsspkg.EndProcedure()

		# Save person fit statistics to new dataset
        if (!is.null(personfitdataset)){
            personfitdict = spssdictionary.CreateSPSSDictionary(c("pattern", gettext("Response pattern",domain=domain), 0, "F4.0", "ordinal"))
            for (i in 1:length(coefficients(res)[,1])){
                personfitdict<-spssdictionary.CreateSPSSDictionary(personfitdict, c(paste("Item_",i,sep=""), gettextf("Item %s",i,domain=domain), 0, "F4.0", "scale"))
            }
			personfitdict<-spssdictionary.CreateSPSSDictionary(personfitdict, 
															   c("L0", gettextf("Levine and Rubin person-fit statistic",domain=domain), 0, "F8.2", "scale"),
															   c("Lz", gettextf("Drasgow person-fit statistic",domain=domain), 0, "F8.2", "scale"),
															   c("p_Lz", gettextf("P-value for Drasgow person-fit statistic",domain=domain), 0, "F8.2", "scale"),
															   c("obs", gettextf("Observed frequency",domain=domain), 0, "F4.0", "scale"),
															   c("exp", gettextf("Expected frequency",domain=domain), 0, "F8.2", "scale"),
															   c("fscores", gettextf("Factor scores",domain=domain), 0, "F8.2", "scale"),
															   c("fscores_se", gettextf("Factor scores standard errors",domain=domain), 0, "F8.2", "scale"),
															   c("residuals", gettextf("Residuals",domain=domain), 0, "F8.2", "scale")
															   )
            tryCatch({
                spssdictionary.SetDictionaryToSPSS(personfitdataset, personfitdict)
                personfitdata <- list()
				personfitdata[[1]] <- seq(1,length(person.fit(res)$resp.patterns[,1]))
                for (i in 2:(length(person.fit(res)$resp.patterns[1,])+1)) {
										personfitdata[[i]] <- person.fit(res)$resp.patterns[,(i-1)]
								}
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+2]] <- person.fit(res)$Tobs[,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+3]] <- person.fit(res)$Tobs[,2]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+4]] <- person.fit(res)$p.values[,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+5]] <- factor.scores(res)$score.dat["Obs"][,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+6]] <- factor.scores(res)$score.dat["Exp"][,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+7]] <- factor.scores(res)$score.dat["z1"][,1]
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+8]] <- factor.scores(res)$score.dat["se.z1"][,1]
				resid <- residuals(res)
				for (i in 1:length(person.fit(res)$resp.patterns[1,])) {
						 resid <- resid[order(resid[,(length(person.fit(res)$resp.patterns[1,])-i+1)]),] 
				}
				personfitdata[[length(person.fit(res)$resp.patterns[1,])+9]] <- resid[,(length(person.fit(res)$resp.patterns[1,])+3)]			
				
                personfitdata = data.frame(personfitdata)
                spssdata.SetDataToSPSS(personfitdataset, personfitdata)
				spssdictionary.EndDataStep()   #1/1/13
                }, 
                error=function(e) {print(e)
                cat(gettext("Failed to create person fit statistics dataset. Dataset name must not already exist: ",domain=domain),personfitdataset)
                }
            )
        }

    }

    res <- tryCatch(rm(list=ls()),warning=function(e){return(NULL)})
    
}

StartProcedure<-function(procname, omsid){
if (as.integer(substr(spsspkg.GetSPSSVersion(),1, 2)) >= 19)
   spsspkg.StartProcedure(procname,omsid)
else
   spsspkg.StartProcedure(omsid)
}

caller<-function(items, missing="listwise", programfile=NULL, execute="true", print_summary=FALSE, print_itemfit=FALSE, plot_descriptives=FALSE,
		   plot_factorscores=FALSE, plot_icc=FALSE, plot_iic=FALSE, personfitdataset=NULL, alpha=NULL){
    
    if(!is.null(programfile)){
        title<-"# SPSSINC RASCH\n"

		if ( !is.null(personfitdataset) ) {
            pfit <- paste("personfitdataset<-",dQuote(personfitdataset),sep="")
		} else {
            pfit <- "personfitdataset<-NULL"			
		}
        lines<-c(title,
            "run_rasch<-",
            attr(run_rasch,"source"),
            paste("items<-",deparse(items),sep=""),
            paste("missing<-",dQuote(missing),sep=""),
            paste("print_summary<-",print_summary,sep=""),
            paste("print_itemfit<-",print_itemfit,sep=""),
            paste("plot_descriptives<-",plot_descriptives,sep=""),
            paste("plot_factorscores<-",plot_factorscores,sep=""),
            paste("plot_icc<-",plot_icc,sep=""),
            paste("plot_iic<-",plot_iic,sep=""),
			pfit,
			"run_rasch(items,missing,print_summary,print_itemfit,plot_descriptives,plot_factorscores,plot_icc,plot_iic,personfitdataset, alpha)")
        f<-file(description=programfile,open="wb",encoding="UTF-8")
        writeLines(lines,con=f)
        close(f)
    }
    
    if (execute=="true") run_rasch(items,missing,print_summary,print_itemfit,plot_descriptives,plot_factorscores,plot_icc,plot_iic,personfitdataset,alpha)
    
}

setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

Run<-function(args){
    cmdname = args[[1]]
    args <- args[[2]]
     oobj<-spsspkg.Syntax(templ=list(
                spsspkg.Template("ITEMS", subc="VARIABLES",  ktype="existingvarlist", var="items", islist=TRUE),
                spsspkg.Template("MISSING", subc="OPTIONS",ktype="str", var="missing"),
                spsspkg.Template("EXECUTE", subc="OPTIONS", ktype="str", var="execute"),
				spsspkg.Template("ALPHA", subc="OPTIONS", ktype="float", var="alpha"),
                spsspkg.Template("SUMMARY", subc="PRINT", ktype="bool", var="print_summary"),
                spsspkg.Template("ITEMFIT", subc="PRINT", ktype="bool", var="print_itemfit"),
                spsspkg.Template("DESCRIPTIVES", subc="PLOT", ktype="bool", var="plot_descriptives"),
                spsspkg.Template("FACTORSCORES", subc="PLOT", ktype="bool", var="plot_factorscores"),
                spsspkg.Template("ICC", subc="PLOT", ktype="bool", var="plot_icc"),
                spsspkg.Template("IIC", subc="PLOT", ktype="bool", var="plot_iic"),
                spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile"),
                spsspkg.Template("PERSONFITDATASET", subc="SAVE", ktype="literal", var="personfitdataset")
                ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else
        res <- spsspkg.processcmd(oobj,args,"caller")
        
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}