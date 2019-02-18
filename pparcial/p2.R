



up   <- f(X[n-1]) *( X[n-1]-X[n-2] )
down <- f(X[n-1]) - f(X[n-2])


X(n) <- X[n-1] - ( up/down )
