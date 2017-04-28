rw2d1 =
  function(n) {
    xpos = ypos = numeric(n)
    truefalse = c(TRUE, FALSE)
    plusminus1 = c(1, -1)
    for(i in 2:n)
          # Decide whether we are moving horizontally
          # or vertically.
      if (sample(truefalse, 1)) {
        xpos[i] = xpos[i-1] + sample(plusminus1, 1)
        ypos[i] = ypos[i-1]
      }
      else {
        xpos[i] = xpos[i-1]
        ypos[i] = ypos[i-1] + sample(plusminus1, 1)
      }
    list(x = xpos, y = ypos)
  }




















rw2d2 =
  # Replace sample with runif()
  function(n) {
    xpos = ypos = numeric(n)
    for(i in 2:n) {
      if (runif(1) > .5) {
        xpos[i] = xpos[i-1] + 2 * (runif(1) > .5) - 1
        ypos[i] = ypos[i-1]
      }
      else {
        xpos[i] = xpos[i-1]
        ypos[i] = ypos[i-1] + 2 * (runif(1) > .5) - 1
      }
    }
    list(x = xpos, y = ypos)
  }















rw2d2.5 =
  # Vectorize the runif() to generate all n in one go.
function(n)
{
    xpos = ypos = numeric(n)
    horOrVert = runif(n) > .5
    delta = 2 * (runif(n) > .5) - 1
    for(i in 2:n) {
      if (horOrVert[i]) {
        xpos[i] = xpos[i-1] + delta[i]
        ypos[i] = ypos[i-1]
      }
      else {
        xpos[i] = xpos[i-1]
        ypos[i] = ypos[i-1] + delta[i]
      }
    }
    list(x = xpos, y = ypos)  
}

rw2d3 =
  # 
  # Here we get rid of the loop.
  # We generate a collection of +1 and -1 values at random
  # Then we generate a collection of TRUE and FALSE values
  # indicating whether this is a horizontal or vertical move.
  # And we get rid of the loop by using the function cumsum()
  # But we can't just cumsum the values in steps. We have to
  # do this conditional on xdir.  But this is where the vectorized
  # function ifelse() is very powerful.
  function(n) {
    steps = 2 * (runif(n - 1) >  .5) - 1
    xdir = runif(n - 1) >  .5
    xpos = c(0, cumsum(ifelse(xdir, steps, 0)))
    ypos = c(0, cumsum(ifelse(xdir, 0, steps)))
    list(x = xpos, y = ypos)
}

rw2d4 =
  # Bring sample back!
  function(n) {
    steps = sample(c(-1, 1), n - 1,
                   replace = TRUE)
    xdir = sample(c(TRUE, FALSE), n - 1,
                  replace = TRUE)
    xpos = c(0, cumsum(ifelse(xdir, steps, 0)))
    ypos = c(0, cumsum(ifelse(xdir, 0, steps)))
    list(x = xpos, y = ypos)
}


rw2d5 =
    # Sample from 4 directions, not horizontally and verticallly
    # separately.
  function(n = 100000) {
    xsteps = c(-1,  1,  0,  0)
    ysteps = c( 0,  0, -1,  1)
    dir = sample(1:4, n - 1, replace = TRUE)
    xpos = c(0, cumsum(xsteps[dir]))
    ypos = c(0, cumsum(ysteps[dir]))
    list(x = xpos, y = ypos)
}

