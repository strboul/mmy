
.PHONY : all
all : install docs check

# Registering native routines for compiled code:
register_routines:
	R -q -e "\
	if(!file.exists('src/init.c')) file.create('src/init.c'); \
	tools::package_native_routine_registration_skeleton('.', con = 'src/init.c'); \
	"

check:
	R -q -e "devtools::check()"

docs:
	R -q -e "devtools::document()"

install: docs
	R -q -e "devtools::install()"
