# Lower Level Regular OLS
model1 <- function(x) {
  lm(exp_share ~ 
               Tylenol25 + Tylenol50 + Tylenol100 + Advil25 + Advil50 + Advil100 + Bayer25 + Bayer50 + # dummies only
               
               Tylenol_25_Tylenol_25_Price + Tylenol_25_Tylenol_50_Price + Tylenol_25_Tylenol_100_Price + # price dummy interactions for tylenol
               Tylenol_50_Tylenol_25_Price + Tylenol_50_Tylenol_50_Price + Tylenol_50_Tylenol_100_Price +
               Tylenol_100_Tylenol_25_Price + Tylenol_100_Tylenol_50_Price + Tylenol_100_Tylenol_100_Price +
               
               Advil_25_Advil_25_Price + Advil_25_Advil_50_Price + Advil_25_Advil_100_Price + # price dummy interactions for advil
               Advil_50_Advil_25_Price + Advil_50_Advil_50_Price + Advil_50_Advil_100_Price + 
               Advil_100_Advil_25_Price + Advil_100_Advil_50_Price + Advil_100_Advil_100_Price + 
               
               Bayer_25_Bayer_25_Price + Bayer_25_Bayer_50_Price + Bayer_25_Bayer_100_Price + # price dummy interactions for bayer
               Bayer_50_Bayer_25_Price + Bayer_50_Bayer_50_Price + Bayer_50_Bayer_100_Price + 
               Bayer_100_Bayer_25_Price + Bayer_100_Bayer_50_Price + Bayer_100_Bayer_100_Price + 
               
               Tylenol_25_Tylenol_Beta + Tylenol_50_Tylenol_Beta + Tylenol_100_Tylenol_Beta + # beta dummy interactions
               Advil_25_Advil_Beta + Advil_50_Advil_Beta + Advil_100_Advil_Beta +
               Bayer_25_Bayer_Beta + Bayer_50_Bayer_Beta + Bayer_100_Bayer_Beta,
             x
)
}

# Lower Level Cost IV
model2 <- function(x) {
  ivreg(exp_share ~ 
                  Tylenol25 + Tylenol50 + Tylenol100 + Advil25 + Advil50 + Advil100 + Bayer25 + Bayer50 + # dummies only
                  
                  Tylenol_25_Tylenol_25_Price + Tylenol_25_Tylenol_50_Price + Tylenol_25_Tylenol_100_Price + # price dummy interactions for tylenol
                  Tylenol_50_Tylenol_25_Price + Tylenol_50_Tylenol_50_Price + Tylenol_50_Tylenol_100_Price +
                  Tylenol_100_Tylenol_25_Price + Tylenol_100_Tylenol_50_Price + Tylenol_100_Tylenol_100_Price +
                  
                  Advil_25_Advil_25_Price + Advil_25_Advil_50_Price + Advil_25_Advil_100_Price + # price dummy interactions for advil
                  Advil_50_Advil_25_Price + Advil_50_Advil_50_Price + Advil_50_Advil_100_Price + 
                  Advil_100_Advil_25_Price + Advil_100_Advil_50_Price + Advil_100_Advil_100_Price + 
                  
                  Bayer_25_Bayer_25_Price + Bayer_25_Bayer_50_Price + Bayer_25_Bayer_100_Price + # price dummy interactions for bayer
                  Bayer_50_Bayer_25_Price + Bayer_50_Bayer_50_Price + Bayer_50_Bayer_100_Price + 
                  Bayer_100_Bayer_25_Price + Bayer_100_Bayer_50_Price + Bayer_100_Bayer_100_Price + 
                  
                  Tylenol_25_Tylenol_Beta + Tylenol_50_Tylenol_Beta + Tylenol_100_Tylenol_Beta + # beta dummy interactions
                  Advil_25_Advil_Beta + Advil_50_Advil_Beta + Advil_100_Advil_Beta +
                  Bayer_25_Bayer_Beta + Bayer_50_Bayer_Beta + Bayer_100_Bayer_Beta | 
                  
                  Tylenol25 + Tylenol50 + Tylenol100 + Advil25 + Advil50 + Advil100 + Bayer25 + Bayer50 + # dummies only
                  
                  Tylenol_25_Tylenol_25_Cost + Tylenol_25_Tylenol_50_Cost + Tylenol_25_Tylenol_100_Cost + # cost dummy interactions for tylenol (IVs)
                  Tylenol_50_Tylenol_25_Cost + Tylenol_50_Tylenol_50_Cost + Tylenol_50_Tylenol_100_Cost +
                  Tylenol_100_Tylenol_25_Cost + Tylenol_100_Tylenol_50_Cost + Tylenol_100_Tylenol_100_Cost +
                  
                  Advil_25_Advil_25_Cost + Advil_25_Advil_50_Cost + Advil_25_Advil_100_Cost + # cost dummy interactions for advil (IVs)
                  Advil_50_Advil_25_Cost + Advil_50_Advil_50_Cost + Advil_50_Advil_100_Cost + 
                  Advil_100_Advil_25_Cost + Advil_100_Advil_50_Cost + Advil_100_Advil_100_Cost + 
                  
                  Bayer_25_Bayer_25_Cost + Bayer_25_Bayer_50_Cost + Bayer_25_Bayer_100_Cost + # cost dummy interactions for bayer (IVs)
                  Bayer_50_Bayer_25_Cost + Bayer_50_Bayer_50_Cost + Bayer_50_Bayer_100_Cost + 
                  Bayer_100_Bayer_25_Cost + Bayer_100_Bayer_50_Cost + Bayer_100_Bayer_100_Cost + 
                  
                  Tylenol_25_Tylenol_Beta + Tylenol_50_Tylenol_Beta + Tylenol_100_Tylenol_Beta + # beta dummy interactions
                  Advil_25_Advil_Beta + Advil_50_Advil_Beta + Advil_100_Advil_Beta +
                  Bayer_25_Bayer_Beta + Bayer_50_Bayer_Beta + Bayer_100_Bayer_Beta, 
                data = x
)
}

# Lower Level with Hausman Instruments
model3 <- function(x) {
  ivreg(exp_share ~ 
                  Tylenol25 + Tylenol50 + Tylenol100 + Advil25 + Advil50 + Advil100 + Bayer25 + Bayer50 + # dummies only
                  
                  Tylenol_25_Tylenol_25_Price + Tylenol_25_Tylenol_50_Price + Tylenol_25_Tylenol_100_Price + # price dummy interactions for tylenol
                  Tylenol_50_Tylenol_25_Price + Tylenol_50_Tylenol_50_Price + Tylenol_50_Tylenol_100_Price +
                  Tylenol_100_Tylenol_25_Price + Tylenol_100_Tylenol_50_Price + Tylenol_100_Tylenol_100_Price +
                  
                  Advil_25_Advil_25_Price + Advil_25_Advil_50_Price + Advil_25_Advil_100_Price + # price dummy interactions for advil
                  Advil_50_Advil_25_Price + Advil_50_Advil_50_Price + Advil_50_Advil_100_Price + 
                  Advil_100_Advil_25_Price + Advil_100_Advil_50_Price + Advil_100_Advil_100_Price + 
                  
                  Bayer_25_Bayer_25_Price + Bayer_25_Bayer_50_Price + Bayer_25_Bayer_100_Price + # price dummy interactions for bayer
                  Bayer_50_Bayer_25_Price + Bayer_50_Bayer_50_Price + Bayer_50_Bayer_100_Price + 
                  Bayer_100_Bayer_25_Price + Bayer_100_Bayer_50_Price + Bayer_100_Bayer_100_Price + 
                  
                  Tylenol_25_Tylenol_Beta + Tylenol_50_Tylenol_Beta + Tylenol_100_Tylenol_Beta + # beta dummy interactions
                  Advil_25_Advil_Beta + Advil_50_Advil_Beta + Advil_100_Advil_Beta +
                  Bayer_25_Bayer_Beta + Bayer_50_Bayer_Beta + Bayer_100_Bayer_Beta | 
                  
                  Tylenol25 + Tylenol50 + Tylenol100 + Advil25 + Advil50 + Advil100 + Bayer25 + Bayer50 + # dummies only
                  
                  Tylenol_25_Tylenol_25_Hausman + Tylenol_25_Tylenol_50_Hausman + Tylenol_25_Tylenol_100_Hausman + # cost dummy interactions for tylenol (IVs)
                  Tylenol_50_Tylenol_25_Hausman + Tylenol_50_Tylenol_50_Hausman + Tylenol_50_Tylenol_100_Hausman +
                  Tylenol_100_Tylenol_25_Hausman + Tylenol_100_Tylenol_50_Hausman + Tylenol_100_Tylenol_100_Hausman +
                  
                  Advil_25_Advil_25_Hausman + Advil_25_Advil_50_Hausman + Advil_25_Advil_100_Hausman + # cost dummy interactions for advil (IVs)
                  Advil_50_Advil_25_Hausman + Advil_50_Advil_50_Hausman + Advil_50_Advil_100_Hausman + 
                  Advil_100_Advil_25_Hausman + Advil_100_Advil_50_Hausman + Advil_100_Advil_100_Hausman + 
                  
                  Bayer_25_Bayer_25_Hausman + Bayer_25_Bayer_50_Hausman + Bayer_25_Bayer_100_Hausman + # cost dummy interactions for bayer (IVs)
                  Bayer_50_Bayer_25_Hausman + Bayer_50_Bayer_50_Hausman + Bayer_50_Bayer_100_Hausman + 
                  Bayer_100_Bayer_25_Hausman + Bayer_100_Bayer_50_Hausman + Bayer_100_Bayer_100_Hausman + 
                  
                  Tylenol_25_Tylenol_Beta + Tylenol_50_Tylenol_Beta + Tylenol_100_Tylenol_Beta + # beta dummy interactions
                  Advil_25_Advil_Beta + Advil_50_Advil_Beta + Advil_100_Advil_Beta +
                  Bayer_25_Bayer_Beta + Bayer_50_Bayer_Beta + Bayer_100_Bayer_Beta, 
                data = data_w_int
)}


