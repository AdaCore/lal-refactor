BUILD_MODE ?= dev
LIBRARY_TYPE ?= static
PROCESSORS ?= 0

ALL_LIBRARY_TYPES = static static-pic relocatable
ALL_BUILD_MODES = dev prod

LIB_PROJECT = gnat/lal_refactor.gpr

BIN_PROJECT = gnat/lal_refactor_driver.gpr

TESTSUITE_PROJECTS = \
	testsuite/ada_drivers/add_parameter/add_parameter.gpr \
	testsuite/ada_drivers/change_parameter_mode/change_parameter_mode.gpr \
	testsuite/ada_drivers/change_parameters_default_value/change_parameters_default_value.gpr \
	testsuite/ada_drivers/change_parameters_type/change_parameters_type.gpr \
	testsuite/ada_drivers/extract_subprogram/extract_subprogram.gpr \
	testsuite/ada_drivers/introduce_parameter/introduce_parameter.gpr \
	testsuite/ada_drivers/move_parameter/move_parameter.gpr \
	testsuite/ada_drivers/pull_up_declaration/pull_up_declaration.gpr \
	testsuite/ada_drivers/refactor_imports/refactor_imports.gpr \
	testsuite/ada_drivers/remove_parameter/remove_parameter.gpr \
	testsuite/ada_drivers/replace_type/replace_type.gpr \
	testsuite/ada_drivers/safe_rename/safe_rename.gpr \
	testsuite/ada_drivers/sort_dependencies/sort_dependencies.gpr \
	testsuite/ada_drivers/suppress_separate/suppress_separate.gpr

.PHONY: all
all: lib bin testsuite_drivers

.PHONY: lib
lib:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprbuild \
			-v \
			-k \
			-XLAL_REFACTOR_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XLAL_REFACTOR_BUILD_MODE=$(BUILD_MODE) \
			-P $(LIB_PROJECT) \
			-p \
			-j$(PROCESSORS) ; \
	done;

.PHONY: bin
bin:
	gprbuild \
		-v \
		-k \
		-XLAL_REFACTOR_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLAL_REFACTOR_BUILD_MODE=$(BUILD_MODE) \
		-P$(BIN_PROJECT) \
		-p \
		-j$(PROCESSORS);

.PHONY: testsuite_drivers
testsuite_drivers:
	for testsuite_project in $(TESTSUITE_PROJECTS) ; do \
		gprbuild \
			-v \
			-k \
			-XLAL_REFACTOR_LIBRARY_TYPE=$(LIBRARY_TYPE) \
			-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
			-XLAL_REFACTOR_BUILD_MODE=$(BUILD_MODE) \
			-P $$testsuite_project \
			-p \
			-j$(PROCESSORS) ; \
	done;

.PHONY: test
test: bin testsuite_drivers
	testsuite/testsuite.py;

.PHONY: clean
clean:
	rm -rf bin lib obj;
	rm -rf testsuite/ada_drivers/bin;
	rm -rf testsuite/ada_drivers/**/obj;

.PHONY: install
install: install-lib install-bin

.PHONY: install-lib
install-lib:
	for library_type in $(ALL_LIBRARY_TYPES) ; do \
		gprinstall \
			-XLAL_REFACTOR_LIBRARY_TYPE=$$library_type \
			-XLIBRARY_TYPE=$$library_type \
			-XLAL_REFACTOR_BUILD_MODE=$(BUILD_MODE) \
			--prefix="$(PREFIX)" \
			--sources-subdir=include/lal-refactor \
			--build-name=$$library_type \
			--build-var=LIBRARY_TYPE \
			-P $(LIB_PROJECT) -p -f ; \
	done ;

.PHONY: install-bin
install-bin:
	gprinstall \
		-XLAL_REFACTOR_LIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XLIBRARY_TYPE=$(LIBRARY_TYPE) \
		-XBUILD_MODE=$(BUILD_MODE) \
		--prefix="$(PREFIX)" \
		-P $(BIN_PROJECT) \
		-p \
		-f ;
