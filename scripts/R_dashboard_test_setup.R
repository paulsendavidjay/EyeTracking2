library(RMySQL)

# GET DATA
risk_data <- read.delim("~/Documents/Academics/Projects/EyeTrackingGaze2/data/cmbd_riskData.txt", header=T, sep="\t")

risk_data_db_frame <- risk_data[c("subjectID", "AgeGroup", "trial", "trialType", "sure1", "sure2", "mag1", "mag2", "cv1", "gamble", "RT", "condition", "outcome", "outlier")]

# ESTABLISH DATABASE CONNECTION
con <- dbConnect(MySQL(),
         user="root", password="",
         dbname="test", host="127.0.0.1")

# CREATE TABLE

# NOT USED HERE, BUT MAY BE USEFUL KNOWLEDGE
createTableString <- dbBuildTableDefinition(con, "riskData", risk_data_db_frame, row.names = TRUE)

dbWriteTable(con, "riskData", risk_data_db_frame)

# DISCONNECT FROM DB
dbDisconnect(con)


