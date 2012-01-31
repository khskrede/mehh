libraries/terminfo_dist-install_VERSION = 0.3.1.3
libraries/terminfo_dist-install_MODULES = System.Console.Terminfo System.Console.Terminfo.Base System.Console.Terminfo.Cursor System.Console.Terminfo.Color System.Console.Terminfo.Edit System.Console.Terminfo.Effects System.Console.Terminfo.Keys
libraries/terminfo_dist-install_HIDDEN_MODULES = 
libraries/terminfo_dist-install_SYNOPSIS =Haskell bindings to the terminfo library.
libraries/terminfo_dist-install_HS_SRC_DIRS = .
libraries/terminfo_dist-install_DEPS = base-4.3.1.0 extensible-exceptions-0.1.1.2
libraries/terminfo_dist-install_DEP_NAMES = base extensible-exceptions
libraries/terminfo_dist-install_INCLUDE_DIRS = 
libraries/terminfo_dist-install_INCLUDES = ncurses.h term.h
libraries/terminfo_dist-install_INSTALL_INCLUDES = 
libraries/terminfo_dist-install_EXTRA_LIBRARIES = ncursesw
libraries/terminfo_dist-install_EXTRA_LIBDIRS = 
libraries/terminfo_dist-install_C_SRCS  = 
libraries/terminfo_dist-install_CMM_SRCS  = $(addprefix cbits/,$(notdir $(wildcard libraries/terminfo/cbits/*.cmm)))
libraries/terminfo_dist-install_DATA_FILES = 
libraries/terminfo_dist-install_HC_OPTS = -Wall -XForeignFunctionInterface -XDeriveDataTypeable -XEmptyDataDecls -XScopedTypeVariables -XFlexibleInstances
libraries/terminfo_dist-install_CC_OPTS = 
libraries/terminfo_dist-install_CPP_OPTS = 
libraries/terminfo_dist-install_LD_OPTS = 
libraries/terminfo_dist-install_DEP_INCLUDE_DIRS = '/home/khs/documents/project/test5/ghc-7.0.3/libraries/base/include' '/home/khs/documents/project/test5/ghc-7.0.3/includes' '/home/khs/documents/project/test5/ghc-7.0.3/libffi/dist-install/build'
libraries/terminfo_dist-install_DEP_CC_OPTS = 
libraries/terminfo_dist-install_DEP_LIB_DIRS = '/home/khs/documents/project/test5/ghc-7.0.3/libraries/extensible-exceptions/dist-install/build' '/home/khs/documents/project/test5/ghc-7.0.3/libraries/base/dist-install/build' '/home/khs/documents/project/test5/ghc-7.0.3/libraries/integer-gmp/dist-install/build' '/home/khs/documents/project/test5/ghc-7.0.3/libraries/ghc-prim/dist-install/build' '/home/khs/documents/project/test5/ghc-7.0.3/rts/dist/build' '/home/khs/documents/project/test5/ghc-7.0.3/libffi/dist-install/build'
libraries/terminfo_dist-install_DEP_EXTRA_LIBS = gmp m rt dl
libraries/terminfo_dist-install_DEP_LD_OPTS = 
libraries/terminfo_dist-install_BUILD_GHCI_LIB = YES