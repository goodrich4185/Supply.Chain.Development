\documentclass[12pt]{article}
\usepackage{amsmath,amssymb,mathrsfs,fancyhdr,syntonly,lastpage,hyperref,enumitem,graphicx}

\hypersetup{colorlinks=true,urlcolor=black}

\topmargin      -1.5cm   % read Lamport p.163
\oddsidemargin  -0.04cm  % read Lamport p.163
\evensidemargin -0.04cm  % same as oddsidemargin but for left-hand pages
\textwidth      16.59cm
\textheight     23.94cm
\parskip         7.2pt   % sets spacing between paragraphs
\parindent         0pt   % sets leading space for paragraphs
\pagestyle{empty}        % Uncomment if don't want page numbers
\pagestyle{fancyplain}

\usepackage{Sweave}

\begin{document}
\lhead{3-2-2014}
\chead{Pyrolyzer Role in Corn Stover Distribution}
\rhead{Ryan Goodrich}

\hline
\textbf{Research questions:}

What set of policies would need to be in place to give cellulosic ethanol a competitive edge over corn based ethanol?

\hline

Notes on "Using GIS and intelligent transportation tools for biomass supply chain modeling and cost assessment" by Slobodan Gutesa, a research assistant with the Iowa State Bioresource and Agricultural Engineering department. \\

Coming soon \\

\hline

Notes on "Stover Harvest Benefits Farmer" by Donnelle Eller, a columnist for the Des Moines Sunday Register.  Can be reached at deller@dmreg.com \\

Bruce Nelson, farmer who collects bales of corn stover for use at Poet Biorefining plant in Emmetsburg.  "The research I've read, one ton is very sustainable."  I.e., they remove around 1 ton of stover per acre from the total which ranges from four or five tons. \\

Poet plant in Emmetsburg is next to an existing 55 million gallon corn-grain ethanol plant.

\hline

Here I gather data from NASS census surveys in 1997, 2002, and 2007 to get an idea of the size of the farms in the corn belt.  Below are the calculations to get a weighted average area harvested in each county, weighted by operation size.

<<Remove Data,echo=FALSE>>=
rm(list=ls())
@

<<Iowa county level corn grain area harvested,warning=FALSE,tidy=FALSE>>=
f<-"U:/Research/Distributional Cost Paper--Dermot/Cornbelt Data.csv"

Harvest.data<-read.table(file=f, header=TRUE,sep=",",stringsAsFactors=FALSE)

operations<-Harvest.data[Harvest.data$Data.Item!="CORN, GRAIN - ACRES HARVESTED",]
operations<-operations[operations$Domain.Category!="AREA HARVESTED: (500 OR MORE ACRES)",]

require(plyr)
ddply(operations,.(County),prop.table(operations$Value,1))


Total_acres_harvested<-subset(Harvest_data,
Data.Item=="CORN, GRAIN - ACRES HARVESTED")

colnames(Total_acres_harvested)[8]<-"Acres.Harvested"

Total_operations<-subset(Harvest_data,
Data.Item=="CORN, GRAIN - OPERATIONS WITH AREA HARVESTED" & Domain == "TOTAL")

colnames(Total_operations)[8]<-"Total.Operations"

require(plyr)
data<-join(x=Total_acres_harvested,
y=Total_operations,
by=c("County","State"),
match="first")

data<-subset(data,
select=c("County","State","Year","Ag.District",
"Acres.Harvested","Total.Operations"))

data$Mean.Acres.Harvested <-round(
as.numeric(data$Acres.Harvested)/as.numeric(data$Total.Operations),
digits=2)

Iowa<-subset(data,State=="IOWA",
select=c("Year","County","Ag.District",
"Acres.Harvested","Total.Operations","Mean.Acres.Harvested"))
head(Iowa)
@

NOTE: the above is not a subset.  I may want to reconsider and eliminate all farm operations under 25 acres or under some other harvested acreage amount. \\

I now turn to getting an idea for the anticipated number of operations in the state given an expected participation percentage.

<<Participants per county in Iowa>>=
rate=0.2
Iowa$Participants<-round(as.numeric(Iowa$Total.Operations)*rate)

@

\end{document}