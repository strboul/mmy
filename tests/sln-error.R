
source("test-helpers.R")

basic <- readRDS(file.path("data", "basic.RDS"))

multiple_expect(mmy::search_list_names(basic, "color"), n = 10, use.gctorture = is_on_ci_platform())
