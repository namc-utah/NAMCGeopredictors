t=read.csv("C:/Users/jenni/OneDrive - USU/Documents/parse.csv")
library(NAMCr)
t=json.expand(t,"metadata")
pred=read.csv("C:/Users/jenni/OneDrive - USU/Documents/predictor_table_for_database.csv")
func=read.csv("C:/Users/jenni/OneDrive - USU/Documents/functions.csv")
library(dplyr)
func$abbreviation=func$predictor
join=left_join(pred,func, by='abbreviation')
write.csv(join,'join.csv')

pred=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/OE_Modeling/Geospatial predictors/predictor_table_for_database.csv")
model=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/OE_Modeling/Geospatial predictors/model_predictors_id.csv")
model$abbreviation=model$predictor_abbreviation
t=left_join(model,pred, by="abbreviation")

historic_pred=read.csv("C:/Users/jenni/Box/NAMC (Trip Armstrong)/OE_Modeling/Geospatial predictors/historic_predictors_for_philip_version_3.csv")
historic_pred$abbreviation=historic_pred$variable
join=left_join(historic_pred,pred,by="abbreviation")
