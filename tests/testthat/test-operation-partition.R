
# basic partition --------------------------------------------------------------

test_that("basic partitions", {
  expect_equal(partition(vars(a)), adjust(aes(partition = a)))
  expect_equal(
    partition(vars(a, b)),
    adjust(aes(partition = interaction(!!quo(a), !!quo(b), drop = TRUE, lex.order = TRUE))),
    ignore_formula_env = TRUE
  )
  expect_equal(partition(~ a), adjust(aes(partition = a)))
  expect_equal(partition(~ a + b), adjust(aes(partition = a + b)))
  expect_equal(
    partition(quote(a)),
    adjust(aes(partition = a)),
    ignore_formula_env = TRUE
  )
})
