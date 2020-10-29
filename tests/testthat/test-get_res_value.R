res <- list(
  a = 1,
  b = "best",
  c = 1:4,
  d = c(d1 = 1, d2 = 2),
  e = list(e1 = 1, e2 = 2)
)

test_that("numeric and logical", {
  expect_equal(get_res_value('TRUE', res), TRUE)
  expect_equal(get_res_value('FALSE', res), FALSE)
  expect_equal(get_res_value('T', res), TRUE)
  expect_equal(get_res_value('F', res), FALSE)
  expect_equal(get_res_value('true', res), TRUE)
  expect_equal(get_res_value('false', res), FALSE)
  expect_equal(get_res_value('1', res), 1)
  expect_equal(get_res_value('-3', res), -3)
  expect_equal(get_res_value('2.5', res), 2.5)
})

test_that("list", {
  expect_equal(get_res_value('a', res), res$a)
  expect_equal(get_res_value('b', res), res$b)
  expect_equal(get_res_value('c', res), res$c)
  expect_equal(get_res_value('d', res), res$d)

  expect_equal(get_res_value('c[2]', res), res$c[[2]])
  expect_equal(get_res_value('d$d2', res), res$d[['d2']])
  expect_equal(get_res_value('d[d2]', res), res$d[['d2']])
  expect_equal(get_res_value('d["d2"]', res), res$d[['d2']])
  expect_equal(get_res_value("d['d2']", res), res$d[['d2']])
  expect_equal(get_res_value('e[e2]', res), res$e[['e2']])
  expect_equal(get_res_value('e$e2', res), res$e$e2)
})
