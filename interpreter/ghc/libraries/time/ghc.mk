libraries/time_PACKAGE = time
libraries/time_dist-install_GROUP = libraries
$(eval $(call build-package,libraries/time,dist-install,$(if $(filter time,$(STAGE2_PACKAGES)),2,1)))
