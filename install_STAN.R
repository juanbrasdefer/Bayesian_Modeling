# installing necessary software for course
# 'Bayesian Modeling' with Sascha Göbel

# install STAN package
install.packages("cmdstanr", 
                 repos = c("https://mc-stan.org/r-packages/", 
                           getOption("repos")))

# load package
library(cmdstanr)

# check C++ Toolchain (whatever that means) 
check_cmdstan_toolchain()
# returns "The C++ toolchain required for CmdStan is setup properly!"
# meaning we can proceed to the next step

# install
install_cmdstan()


#* Finished installing CmdStan to /Users/Juan/.cmdstan/cmdstan-2.36.0
#CmdStan path set to: /Users/Juan/.cmdstan/cmdstan-2.36.0