
s1 <- git2r::status()
stopifnot(
  vapply(s1, length, integer(1L)) == 0L
)
usethis::use_mit_license()
unlink(".github", recursive = TRUE, force = TRUE)
usethis::use_github_action_check_release()
s2 <- git2r::status()

if (!identical(s1, s2)) {
  git2r::add(path = ".")
  git2r::commit(message = "build: run dev/01_license_and_ci.R")
}
