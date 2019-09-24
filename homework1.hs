module A1 where


-- | Computes a quadratic, with a, b, and c being the values of the equation
-- and x being the multiplier
  quadratic a b c x = a + b * x + c*x^2

-- | Scales a vector (b,c) by the factor of a.
  scaleVector a (b,c) = (a*b, a*c)

-- | Finds the distance in three dimensional space between (a,b,c) and (d,e,f,)
  tripleDistance (a,b,c) (d,e,f) = sqrt((d-a)^2 + (e-b)^2 +(f-c)^2)
