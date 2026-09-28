# Extracted from test-M2-diagnostics.R:18

# test -------------------------------------------------------------------------
txt <- readLines(path_in("reports", "diagnostics.md"))
sections <- split(txt, cumsum(grepl("^## ", txt)))
numbered <- sections[vapply(sections, function(s) grepl("^## [1-5]\\.", s[1]), logical(1))]
expect_length(numbered, 5)
for (s in numbered) {
    body <- paste(s, collapse = " ")
    expect_true(grepl("Decision", body), info = s[1])
    cited <- regmatches(body, gregexpr("m2_[a-z_]+\\.(csv|png)", body))[[1]]
    expect_gt(length(cited), 0)
    for (f in unique(cited)) {
      loc <- if (grepl("png$", f)) path_in("outputs", "figures", f) else path_in("outputs", f)
      expect_true(file.exists(loc), info = f)
    }
  }
