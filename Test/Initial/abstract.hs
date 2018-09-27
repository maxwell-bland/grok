sumsize :: List ( List a ) -> Int
sumsize Emp = 0
sumsize ( x :+: xs ) = size x + sumsize xs

concat :: x:List ( List a ) -> {v :List a | size v = sumsize x}
concat Emp = Emp
concat ( xs :+: Emp) = xs
concat ( xs :+: ( ys :+: xss )) = concat (( append xs ys ) :+: xss )

append :: List a -> List a -> List a
append Emp Emp = Emp
append xs Emp = xs
append Emp ys = ys
append ( x :+: xs ) ys = x :+: append xs ys