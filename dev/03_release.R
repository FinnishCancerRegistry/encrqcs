
message("select version element to bump: [major/minor/patch]")
desc::desc_bump_version(which = readline(": "))

system2("git", c("add", "DESCRIPTION"))
v <- as.character(desc::desc_get_version())
system2("git", c("commit", paste0("-m \"build: v", v, "\"")))

system2("git", c("tag", paste0("v", v)))

system2("git", c("push", "--tags"))
system2("git", c("push"))
