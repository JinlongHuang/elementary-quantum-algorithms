-- | initialize n_qubit_oracle’s function
n_qubit_oracle_function :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
n_qubit_oracle_function (controlled_qubit, target_qubit) = do
  qnot_at target_qubit `controlled` controlled_qubit .==. [1,0,1,1,1]
  return (controlled_qubit, target_qubit)