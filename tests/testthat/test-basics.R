context("test-basics")

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

create_distances <- function(groupSize, groupNumber, groupDistance) {
  N <- groupSize * groupNumber
  m <- matrix(runif(N*N), nrow=N, ncol=N)
  for(i in 1:N) {
    for(j in 1:i) {
      if(i==j) {
        m[i,j] <- 0
      } else if((i-1)%%groupSize == (j-1)%%groupSize) {
        m[i,j] <- runif(1)
        m[j,i] <- m[i,j]
      } else {
        m[i,j] <- groupDistance + runif(1);
        m[j,i] <- m[i,j]
      }
    }
  }
  return(m)
}


test_that("size works for constructed distanceMatrices", {
  for(nGroup in 5:10) {
    for(nGroups in 5:10) {
      m <- create_distances(nGroup, nGroups, 3.0)
      dm <- dist(m)
      cc <- quiet(CoreCollection(dm, nGroups))
      expect_equal(nrow(cc$core), nGroups)
    }
  }
})

test_that("seed works", {
  nGroup <- 5
  nGroups <- 10
  seed <- 1234567
  m <- create_distances(nGroup, nGroups, 3.0)
  dm <- dist(m)
  n <- nGroup * nGroups
  comparisonWithoutSeed = TRUE
  #compute cores of multiple sizes
  for(i in (1+1):(n-1)) {
    cc1 <- quiet(CoreCollection(dm, i, seed=seed))
    cc2 <- quiet(CoreCollection(dm, i, seed=seed))
    cc3 <- quiet(CoreCollection(dm, i))
    expect_identical(cc1$core, cc2$core)
    comparisonWithoutSeed <- comparisonWithoutSeed & identical(cc1$core, cc3$core)
  }
  #but this should (almost certainly) be false
  expect_false(comparisonWithoutSeed)
})
