libraries/random_PACKAGE = random
libraries/random_dist-install_GROUP = libraries
$(eval $(call build-package,libraries/random,dist-install,$(if $(filter random,$(STAGE2_PACKAGES)),2,1)))
