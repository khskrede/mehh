libraries/bytestring_dist-install_VERSION = 0.9.1.10
libraries/bytestring_dist-install_MODULES = Data.ByteString Data.ByteString.Char8 Data.ByteString.Unsafe Data.ByteString.Internal Data.ByteString.Lazy Data.ByteString.Lazy.Char8 Data.ByteString.Lazy.Internal Data.ByteString.Fusion
libraries/bytestring_dist-install_HIDDEN_MODULES = 
libraries/bytestring_dist-install_SYNOPSIS =Fast, packed, strict and lazy byte arrays with a list interface
libraries/bytestring_dist-install_HS_SRC_DIRS = .
libraries/bytestring_dist-install_DEPS = base-4.3.1.0 ghc-prim-0.2.0.0
libraries/bytestring_dist-install_DEP_NAMES = base ghc-prim
libraries/bytestring_dist-install_INCLUDE_DIRS = include
libraries/bytestring_dist-install_INCLUDES = fpstring.h
libraries/bytestring_dist-install_INSTALL_INCLUDES = fpstring.h
libraries/bytestring_dist-install_EXTRA_LIBRARIES = 
libraries/bytestring_dist-install_EXTRA_LIBDIRS = 
libraries/bytestring_dist-install_C_SRCS  = cbits/fpstring.c
libraries/bytestring_dist-install_CMM_SRCS  = $(addprefix cbits/,$(notdir $(wildcard libraries/bytestring/cbits/*.cmm)))
libraries/bytestring_dist-install_DATA_FILES = 
libraries/bytestring_dist-install_HC_OPTS = -fdicts-cheap -Wall -fno-warn-orphans -O2 -funbox-strict-fields -fno-method-sharing -fmax-simplifier-iterations10 -XUnliftedFFITypes -XMagicHash -XUnboxedTuples -XDeriveDataTypeable -XScopedTypeVariables -XNamedFieldPuns -XCPP -XForeignFunctionInterface
libraries/bytestring_dist-install_CC_OPTS = 
libraries/bytestring_dist-install_CPP_OPTS = 
libraries/bytestring_dist-install_LD_OPTS = 
libraries/bytestring_dist-install_DEP_INCLUDE_DIRS = '/home/khs/documents/project/test5/ghc-7.0.3/libraries/base/include' '/home/khs/documents/project/test5/ghc-7.0.3/includes' '/home/khs/documents/project/test5/ghc-7.0.3/libffi/dist-install/build'
libraries/bytestring_dist-install_DEP_CC_OPTS = 
libraries/bytestring_dist-install_DEP_LIB_DIRS = '/home/khs/documents/project/test5/ghc-7.0.3/libraries/base/dist-install/build' '/home/khs/documents/project/test5/ghc-7.0.3/libraries/integer-gmp/dist-install/build' '/home/khs/documents/project/test5/ghc-7.0.3/libraries/ghc-prim/dist-install/build' '/home/khs/documents/project/test5/ghc-7.0.3/rts/dist/build' '/home/khs/documents/project/test5/ghc-7.0.3/libffi/dist-install/build'
libraries/bytestring_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/bytestring_dist-install_DEP_LD_OPTS = 
libraries/bytestring_dist-install_BUILD_GHCI_LIB = YES