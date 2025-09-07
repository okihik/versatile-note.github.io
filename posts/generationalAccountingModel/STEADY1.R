# ==============================================================================
# Function to Calculate the Initial Steady-State Capital-Labor Ratio
# Corresponds to STEADY1.m
# ==============================================================================

STEADY1 <- function(XKL0) {
  
  # ---
  # 1. Initialize variables for the loop
  # ---
  OLDX <- XKL0
  SDIF <- 1.0
  SKOUNT <- 0
  
  # Get global variables defined in the main script
  # Note: In a real R project, it's better to pass these as arguments 
  # or store them in a list, but we follow the book's global variable style.
  
  # ---
  # 2. Start the convergence loop
  # ---
  while (SKOUNT < ITRTE && SDIF > (1 - DELTA)) {
    
    XKL <- OLDX
    XNN <- XNN1 # Use the initial steady-state population growth
    
    # ---
    # 3. Calculate Factor Prices (Wages and Interest Rate)
    # ---
    W <- (1 - EPSI) * A * XKL^EPSI
    R <- EPSI * A * XKL^(EPSI - 1) - RDEP
    
    # ---
    # 4. Simplify variables for household calculations
    # ---
    RN <- R * (1 - TR)
    XNG <- (1 + XNN) * (1 + GG) - 1
    
    # ---
    # 5. Household Behavior Calculation
    # ---
    
    # Calculate the present value of lifetime labor supply (1-period value)
    DIS1 <- 0
    for (I in 1:IRET) {
      DIS1 <- DIS1 + ((1 + RN)^(I - 1)) * ((1 + GG)^(-I)) * SL[I]
    }
    
    # Calculate the present value of lifetime consumption (1-period value, tax-inclusive)
    DIS2 <- 0
    for (I in 1:IDIE) {
      DIS2 <- DIS2 + (((1 + RN) / (1 + RHO))^((I - 1) / GAMMA)) * ((1 + RN)^(I - 1)) * (1 + TC)
    }
    
    # Consumption in the first period of life
    C1 <- W * (1 - TW) * DIS1 / DIS2
    
    # Consumption stream over the lifecycle
    C <- numeric(IDIE)
    for (J in 1:IDIE) {
      C[J] <- (((1 + RN) / (1 + RHO))^((J - 1) / GAMMA)) * C1
    }
    
    # Asset accumulation stream over the lifecycle
    AA <- numeric(IDIE)
    WX <- numeric(IDIE)
    WX[1:IRET] <- W # Wage is earned only during working years
    
    AA[1] <- WX[1] * SL[1] * (1 - TW) - C[1] * (1 + TC)
    for (J in 2:IDIE) {
      AA[J] <- AA[J - 1] * (1 + RN) + ((1 + GG)^(J - 1)) * WX[J] * SL[J] * (1 - TW) - C[J] * (1 + TC)
    }
    
    # ---
    # 6. Macroeconomic Aggregation
    # ---
    
    # Aggregate private financial assets (PASET)
    PASET <- 0
    for (J in 1:IDIE) {
      PASET <- PASET + (1 + XNG)^(1 - J) * AA[J] * GEN
    }
    
    # Aggregate labor supply (XL)
    XL <- 0
    for (J in 1:IRET) {
      XL <- XL + SL[J] * (1 + XNN)^(1 - J) * GEN
    }
    
    # Aggregate private financial assets for the next period (PASBAK)
    PASBAK <- PASET / (1 + XNG)
    
    # ---
    # 7. Calculate the new Capital-Labor Ratio
    # ---
    X <- (1 - SDRT) * PASBAK / XL
    
    # ---
    # 8. Check for convergence
    # ---
    SDIF <- abs(1 - X / OLDX)
    OLDX <- 0.5 * (X + OLDX) # Update the guess (midpoint method)
    SKOUNT <- SKOUNT + 1
    
    # Optional: Print progress
    # cat(sprintf("Iteration: %d, XKL: %.4f, SDIF: %.8f\n", SKOUNT, OLDX, SDIF))
  }
  
  # ---
  # 9. Return the converged value
  # ---
  return(OLDX)
}

print("STEADY1 function has been defined.")