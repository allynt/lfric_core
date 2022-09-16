# Object dependencies
$(MOD_DIR)/delta_mod.mod : objects/gamma_mod.o
$(MOD_DIR)/alpha_mod.mod : objects/dir/alpha_mod.o
$(MOD_DIR)/zeta_mod.mod : objects/dir/zeta_mod.o
$(MOD_DIR)/epsilon_mod.mod : objects/epsilon_mod.o
$(MOD_DIR)/epsilon_one_submod.mod : objects/epsilon_one_submod.o
$(MOD_DIR)/epsilon_two_submod.mod : objects/epsilon_two_submod.o
$(MOD_DIR)/eta_mod.mod : objects/eta_mod.o
$(MOD_DIR)/gamma_mod.mod : objects/gamma_mod.o
$(MOD_DIR)/mismatched_mod.mod : objects/beta_mod.o
objects/beta_mod.o : $(MOD_DIR)/delta_mod.mod
objects/dir/alpha_mod.o : $(MOD_DIR)/eta_mod.mod
objects/epsilon_one_submod.o : $(MOD_DIR)/epsilon_mod.mod $(MOD_DIR)/eta_mod.mod
objects/epsilon_two_submod.o : $(MOD_DIR)/epsilon_mod.mod
objects/one.o : $(MOD_DIR)/alpha_mod.mod $(MOD_DIR)/gamma_mod.mod $(MOD_DIR)/epsilon_mod.mod
objects/two.o : $(MOD_DIR)/mismatched_mod.mod
