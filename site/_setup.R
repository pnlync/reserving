# Shared setup for every site page: numbers from outputs/, figures copied into assets/.
root <- normalizePath("..")
out <- function(f) file.path(root, "outputs", f)
cv <- jsonlite::read_json(out("cv_numbers.json"))
d <- cv$display
dir.create("assets", showWarnings = FALSE)
fig <- function(f) {
  file.copy(out(file.path("figures", f)), file.path("assets", f), overwrite = TRUE)
  file.path("assets", f)
}
invisible(file.copy(file.path(root, "reports", "reserving_memo.pdf"), "assets/reserving_memo.pdf", overwrite = TRUE))
mm <- function(x) formatC(x / 1000, format = "f", digits = 1)
pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")
