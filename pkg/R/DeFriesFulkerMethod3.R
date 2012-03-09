DeFriesFulkerMethod3 <-
function( outcomeForSubject1, outcomeForSubject2, relatedness ) { 
  dv_1Centered <- scale(outcomeForSubject1, center=TRUE, scale=FALSE)
  dv_2Centered <- scale(outcomeForSubject2, center=TRUE, scale=FALSE)
  interaction <- dv_2Centered*relatedness    
  
  brief <- summary(lm(dv_1Centered ~ 0 + dv_2Centered + interaction)) #The '0' specifies and intercept-free model.
  
  coeficients <- coef(brief)
  nDouble <- length(brief$residuals) 
  b1 <- coeficients["dv_2Centered", "Estimate"]  
  b2 <- coeficients["interaction", "Estimate"]
  eSquared <- 1 - (b1+b2)
  
  return( list(HSquared=b2, CSquared=b1, ESquared=eSquared, RowCount=nDouble) )
}
