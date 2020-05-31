# Setup  -----------------------------------------------------------------------
ci_types = standard conditional unconditional hybrid
conf_ints := $(foreach type,$(ci_types),confidence_intervals_$(type).rds)

# Functions --------------------------------------------------------------------
# Run and log an R script
r = Rscript --vanilla --verbose $(strip $(1)) > log/$(basename $(strip $(1))).log 2>&1

# Return just a file name: "dir/script.R" -> "script"
file_slug = $(basename $(notdir $(1)))

# Split a file slug into words by underscore
split_slug = $(subst _, ,$(call file_slug,$(1)))

# Extract the nth element from a file slug
slug_segment = $(word $(1),$(call split_slug,$(2)))

# Extract elements of the file slug by name
extract_ci_type = $(call slug_segment,3,$(1))

# Print a variable
# Run "make print-..."
print-% : ; @echo $* = $($*)

# ALL --------------------------------------------------------------------------
all: log
all: simulated_experiments.rds
all: $(conf_ints)
all: coverage.rds
all: conditional_coverage.png


# Rules ------------------------------------------------------------------------
log:
	mkdir -p log

simulated_experiments.rds: 01_simulate-experiments.R
	$(call r, $<)


define build_conf_int

$(1): simulated_experiments.rds
$(1): 02_calculate-intervals.R
	Rscript --vanilla --verbose 02_calculate-intervals.R \
	--ci_type $(call extract_ci_type, $(1)) \
	> log/02_calculate-intervals_$(call extract_ci_type, $(1)).log 2>&1

endef

$(foreach ci,$(conf_ints),$(eval $(call build_conf_int,$(ci))))


coverage.rds: $(conf_ints)
coverage.rds: 03_measure-coverage.R
	$(call r, $<)

conditional_coverage.png: coverage.rds
conditional_coverage.png: 04_plot-conditional-coverage.R
	$(call r, $<)
