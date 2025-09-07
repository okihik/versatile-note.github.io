# ==============================================================================
# Function to Calculate Lifetime Utility for Equivalent Variation
# Corresponds to UDIF1.m
# ==============================================================================

UDIF1 <- function(EV, U, I) {
  
  # This function calculates the difference between a generation's utility (U)
  # and the utility they would have had if they lived in the initial steady
  # state but with their consumption scaled by a factor EV (Equivalent Variation).
  
  UREF <- 0
  
  # We need the steady-state consumption profile (SC) calculated earlier.
  # In a more robust setup, we would pass SC as an argument.
  # For now, we assume it's available in the global environment.
  
  if (GAMMA == 1) {
    # Log utility case
    for (J in 1:IDIE) {
      UREF <- UREF + log(SC[J] * (1 + GG)^I * EV) * (1 + RHO)^(-(J - 1))
    }
  } else {
    # CES utility case
    for (J in 1:IDIE) {
      UREF <- UREF + ((SC[J] * (1 + GG)^I * EV)^(1 - GAMMA)) / (1 - GAMMA) * (1 + RHO)^(-(J - 1))
    }
  }
  
  # Return the difference
  F <- UREF - U
  return(F)
}

print("UDIF1 function has been defined.")
