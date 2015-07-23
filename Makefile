APP := dstwkr

export EXOMETER_PACKAGES="(minimal)"

include make/vars.mk
include make/common.mk
include make/dialyzer.mk
include make/rebar.mk
include make/release.mk
include make/pkg.mk
