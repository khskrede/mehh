libraries/integer-gmp_PACKAGE = integer-gmp
libraries/integer-gmp_dist-install_GROUP = libraries
$(eval $(call build-package,libraries/integer-gmp,dist-install,$(if $(filter integer-gmp,$(STAGE2_PACKAGES)),2,1)))
