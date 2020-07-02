module Main where

import Numeric.NLOPT
import Numeric.LinearAlgebra (fromList, toList)

main :: IO ()
main = do
    let objf x = let [x1, x2, x3] = toList x
                 in 3 * x1 * x1 + 2 * x1 * x2 + x1 * x3
                    + 2.5 * x2 * x2 + 2 * x2 * x3 + 2 * x3 * x3
                    - 8 * x1 - 3 * x2 - 3 * x3

        objfGrad x = let [x1, x2, x3] = toList x
                     in fromList [ 6 * x1 + 2 * x2 + x3 - 8
                                 , 2 * x1 + 5 * x2 + 2 * x3 - 3
                                 , x1 + 2 * x2 + 4 * x3 - 3 ]

        objfD x = (objf x, objfGrad x)

        stop = ObjectiveRelativeTolerance 1e-9 :| []

        c1f x = let [x1, _, x3] = toList x in x1 + x3 - 3
        c1fGrad _ = fromList [1, 0, 1]
        c1fD x = (c1f x, c1fGrad x)
        c1 = EqualityConstraint (Scalar c1fD) 1e-6

        c2f x = let [_, x2, x3] = toList x in x2 + x3
        c2fGrad _ = fromList [0, 1, 1]
        c2fD x = (c2f x, c2fGrad x)
        c2 = EqualityConstraint (Scalar c2fD) 1e-6

        algorithm = SLSQP objfD [] [] [c1, c2]
        problem = LocalProblem 3 stop algorithm

        x0 = fromList [0, 0, 0]

        solC = minimizeLocal problem x0

    print solC

    putStrLn "-- Done."
