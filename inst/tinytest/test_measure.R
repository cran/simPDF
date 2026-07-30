## Measurement is AFM-exact and linear in size (fractional included).
out <- tempfile(fileext = ".pdf")
doc <- sp_new(out, paper = "letter", family = "Helvetica", size = 10)

# Courier is monospaced: width == nchar * 0.6 * size, exactly, for any size.
expect_equal(sp_width(doc, "XXXXXXXXXX", size = 10, family = "mono"), 60)
expect_equal(sp_width(doc, "XXXXXXXXXX", size = 12, family = "mono"), 72)
expect_equal(sp_width(doc, "XXXXXXXXXX", size = 3.533, family = "mono"),
             6 * 3.533, tolerance = 1e-9)          # fractional size exact (ref-scaled)

# Proportional font differs from monospace.
expect_true(abs(sp_width(doc, "mmmmmmmmmm", 10, family = "sans") - 60) > 1)

# Vectorized and monotone.
v <- sp_width(doc, c("a", "abc", "abcdef"), 10, family = "mono")
expect_equal(length(v), 3L)
expect_true(v[1] < v[2] && v[2] < v[3])

# Line pitch scales linearly with size.
expect_equal(sp_height(doc, 20), 2 * sp_height(doc, 10))
sp_close(doc)
