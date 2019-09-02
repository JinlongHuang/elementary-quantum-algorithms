-- This file is firstly coded by authors of arXiv:1406.4481v2, and then modified
-- by qWalker. Copyright (C) 2016. All rights reserved.
--
-- ============================================================================

import Quipper

-- import modules for oracle implementation
import Quantum.Synthesis.Matrix
import QuipperLib.Synthesis
import Quantum.Synthesis.Ring

-- Start Circuits Generation====================================================
-- define oracle data type
data Oracle = Oracle {
  qubit_num :: Int,
  function :: ([Qubit], [Qubit]) -> Circ ([Qubit], [Qubit])
}

-- define simon_circuit function
simon_circuit :: Oracle -> Circ ([Bit], [Bit])
simon_circuit oracle = do
  --create the ancillaes
  top_qubits <- qinit (replicate (qubit_num oracle) False)
  bottom_qubits <- qinit (replicate (qubit_num oracle) True)
  label (top_qubits, bottom_qubits) ("top |0>", "bottom |1>")
  -- apply first hadamard gate
  mapUnary hadamard top_qubits
  mapUnary hadamard bottom_qubits
  -- call the oracle
  (function oracle) (top_qubits, bottom_qubits)
  -- apply hadamard gate again
  mapUnary hadamard top_qubits
  -- measure qubits
  (top_qubits, bottom_qubits) <- measure(top_qubits, bottom_qubits)
  -- return the result
  return (top_qubits,bottom_qubits)

-- define steps function
steps :: (Oracle -> Circ ([Bit], [Bit])) -> Oracle -> Circ (Maybe ([Bit], [Bit]))
steps simon_algorithm oracle = do
  comment " Simon’s algorithm"
  -- set value for n
  let n = toEnum (qubit_num oracle) :: Int
  -- call simon_circuit n-1 times
  for 1 (n-1) 1 $ \i -> do
    comment "start"
    -- call simon_circuit function
    ret <- simon_algorithm oracle
    -- return the result
    return ret
    comment "finish"
  endfor
  return Nothing
  
-- define main function
main = print_generic Preview (steps simon_circuit sample_oracle)

-- End Circuits Generation======================================================


-- Start Oracle examples========================================================
-- define sample_oracle’s data type
sample_oracle :: Oracle
sample_oracle = Oracle{
  qubit_num = 2,
  function = sample_function
}
-- initialize sample_oracle’s function

sample_function :: ([Qubit],[Qubit]) -> Circ ([Qubit],[Qubit])
sample_function (controlled_qubit, target_qubit) = do
  let element = controlled_qubit ++ target_qubit
  -- call the unitary matrix
  exact_synthesis operator element
  return (controlled_qubit, target_qubit)

-- initialize 16 by 16 unitary matrix
operator :: Matrix Sixteen Sixteen DOmega
operator = matrix16x16 ( 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 )
                       ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0 )
           
-- End Oracle examples==========================================================
