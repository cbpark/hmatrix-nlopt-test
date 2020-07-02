module Main where

import Numeric.NLOPT
-- import Numeric.LinearAlgebra (dot)
import Numeric.LinearAlgebra (fromList, toList)

main :: IO ()
main = do
    let -- objf x = x `dot` x + 22
        -- objf x = sum (toList x)
        objf x = let [x1, x2, x3] = toList x
                 in 3 * x1 * x1 + 2 * x1 * x2 + x1 * x3
                    + 2.5 * x2 * x2 + 2 * x2 * x3 + 2 * x3 * x3
                    - 8 * x1 - 3 * x2 - 3 * x3

        stop = ObjectiveRelativeTolerance 1e-9 :| []
        -- algorithm = SBPLX objf [] Nothing
        algorithm = NELDERMEAD objf [] Nothing
        subproblem = LocalProblem 3 stop algorithm
        x0 = fromList [0, 0, 0]

        solU = minimizeLocal subproblem x0

    print solU

    -- define constraint function:
    let -- constraintf x = sum (toList x) - 1.0
        -- constraintf x = let [x1, x2] = toList x
        --                 in x1 * x1 + x2 * x2 - 2
        c1f x = let [x1, _, x3] = toList x
                in x1 + x3 - 3
        c2f x = let [_, x2, x3] = toList x
                in x2 + x3

    -- define constraint object to pass to the algorithm:
    let -- constraint = EqualityConstraint (Scalar constraintf) 1e-6
        c1 = EqualityConstraint (Scalar c1f) 1e-6
        c2 = EqualityConstraint (Scalar c2f) 1e-6
        -- problem = AugLagProblem [constraint] [] (AUGLAG_EQ_LOCAL subproblem)
        problem = AugLagProblem [c1, c2] [] (AUGLAG_EQ_LOCAL subproblem)

        -- x = (2, -1, 1)
        solC = minimizeAugLag problem x0

    print solC

    putStrLn "-- Done."
