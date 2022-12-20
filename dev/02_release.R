
message("which number to bump in version? [major/minor/patch]")
desc::desc_bump_version(readline(": "))

message("automatically tag version? [y/n]")
if (readline(": ") == "y") {
  s1 <- git2r::status()
  git2r::add(path = "DESCRIPTION")
  s2 <- git2r::status()
  if (!identical(s1, s2)) {
    new_v <- desc::desc_get_version()
    system2("git", c("commit", paste0("-m \"build: v", new_v, "\"")))
    system2("git", c("tag", paste0("v", new_v)))
  }
}

message("push commits and tags? [y/n]")
if (readline(": ") == "y") {
  system2("git", c("push"))
  system2("git", c("push", "--tags"))
}
