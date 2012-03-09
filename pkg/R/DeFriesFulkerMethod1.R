DeFriesFulkerMethod1 <-
function( outcomeForSubject1, outcomeForSubject2, relatedness ) {   
  brief <- summary(lm(outcomeForSubject1 ~ 1 + outcomeForSubject2 + relatedness + outcomeForSubject2*relatedness))
  coeficients <- coef(brief)
  nDouble <- length(brief$residuals) 
  #b0 <- coeficients["(Intercept)", "Estimate"]
  b1 <- coeficients["outcomeForSubject2", "Estimate"]  
  #b2 <- coeficients["R", "Estimate"]
  b3 <- coeficients["outcomeForSubject2:relatedness", "Estimate"]
  eSquared <- 1 - (b1+b3)
  
  return( list(HSquared=b3, CSquared=b1, ESquared=eSquared, RowCount=nDouble) )
}
