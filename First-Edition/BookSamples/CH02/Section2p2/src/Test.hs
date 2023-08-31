{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Test where

import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Data.Proxy ( Proxy(..) )


newtype T (rule :: Symbol) = T String       -- A

class
  KnownSymbol rule =>                       -- B
  A (rule :: Symbol) where                  -- C
  step :: T rule -> T rule
  name
    ::                                      -- D
    T rule -> String                        -- E
  name _ = symbolVal (Proxy :: Proxy rule)


{-

A1: rule
A2: (rule :: Symbol)

B1: none
B2: KnownSymbol rule =>

C1: rule
C2: (rule:: Symbol)

D1: none
D2: KnownSymbol

E1: rule
E2: (rule :: Symbol)

T1	A1	B1	C1	D1	E1
T2	A1	B1	C1	D1	E2
T3	A1	B1	C1	D2	E1
T4	A1	B1	C1	D2	E2
T5	A1	B1	C2	D1	E1
T6	A1	B1	C2	D1	E2
T7	A1	B1	C2	D2	E1
T8	A1	B1	C2	D2	E2
T9	A1	B2	C1	D1	E1
T10	A1	B2	C1	D1	E2
T11	A1	B2	C1	D2	E1
T12	A1	B2	C1	D2	E2
T13	A1	B2	C2	D1	E1
T14	A1	B2	C2	D1	E2
T15	A1	B2	C2	D2	E1
T16	A1	B2	C2	D2	E2
T17	A2	B1	C1	D1	E1
T18	A2	B1	C1	D1	E2
T19	A2	B1	C1	D2	E1
T20	A2	B1	C1	D2	E2
T21	A2	B1	C2	D1	E1
T22	A2	B1	C2	D1	E2
T23	A2	B1	C2	D2	E1
T24	A2	B1	C2	D2	E2
T25	A2	B2	C1	D1	E1
T26	A2	B2	C1	D1	E2
T27	A2	B2	C1	D2	E1
T28	A2	B2	C1	D2	E2
T29	A2	B2	C2	D1	E1
T30	A2	B2	C2	D1	E2
T31	A2	B2	C2	D2	E1
T32	A2	B2	C2	D2	E2



-}





